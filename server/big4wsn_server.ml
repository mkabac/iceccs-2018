(* Sever receiving events from clients. Depending on the event type, the global
   state of the wsn (represented by a bigraph) is updated. *)

open Lwt
open Events
open Bigraph
module T = ANSITerminal

let version = "v0.1"

(* Default values *)
module Defaults = struct
  let listen_address = Unix.inet_addr_loopback
  let port = 9000
  let backlog = 50
  let h_size = 10_000
  let path = Filename.concat "/tmp/" (Misc.path (Unix.gettimeofday ()))
  let t_dumpd = 60.
  let csv_log = "events.csv"
end

module State = struct

  type t =
    { h : (int, Big.t) Hashtbl.t;   (* Map from state ids to bigraphs      *)
      l : (int, int) Hashtbl.t;      (* Map from state ids to predicate ids *)
      p : (int, Big.t) Hashtbl.t;   (* Map from predicate ids to bigraphs  *)
      t0 : float;                    (* Creation time                       *)
      t : float;                     (* Last access time                    *)
      last_dump : int option;        (* Last state saved                    *)
      path : string;                 (* Dump path                           *)
    }

  let init path =
    let t = Unix.gettimeofday () in
    { h = Hashtbl.create ~random:true Defaults.h_size;
      l = Hashtbl.create ~random:true Defaults.h_size;
      p = Hashtbl.create ~random:true (Defaults.h_size / 100);
      t0 = t;
      t = t;
      last_dump = None;
      path = path;
    }

  let string_of s =
    "  Number states:\t" ^ (string_of_int @@ Hashtbl.length s.h) ^ "\n"
    ^ "  Number predicates:\t" ^ (string_of_int @@ Hashtbl.length s.p) ^ "\n"
    ^ "  Creation time:\t" ^ (Misc.timestamp s.t0) ^ "\n"
    ^ "  Last access time:\t" ^ (Misc.timestamp s.t) ^ "\n"
    ^ "  Last dumped state:\t"
    ^ (match s.last_dump with
        | None -> "-"
        | Some i -> string_of_int i)

  (* Register a list of predicates *)
  let register ps s =
    List.iter (fun p ->
        Hashtbl.(add s.p (length s.p) p)) ps

  let update b s =
    (* Add new state *)
    let b_index = Hashtbl.length s.h in
    Hashtbl.add s.h b_index b;
    (* Check predicates *)
    Hashtbl.iter (fun i p ->
        if Big.occurs ~target:b ~pattern:p then
          Hashtbl.add s.l b_index i) s.p;
    (* Update last access time *)
    { s with t = Unix.gettimeofday (); }

  let current s =
    let index = (Hashtbl.length s.h) - 1 in
    (Hashtbl.find s.h index, index)

  let label s i =
    Hashtbl.find_all s.l i

  let _async f_write b name path =
    Lwt.async
      (fun () -> Lwt.catch
          (fun () -> Lwt.wrap (fun () ->
               ignore @@ f_write b ~name ~path) >>= fun () ->
             (* A bit verbose. We can disable this later. *)
             Lwt_io.write_line Lwt_io.stdout
               ("  File " ^ (Filename.concat path name) ^ " written succesfully"))
          (fun ex ->
             Lwt_io.write_line Lwt_io.stderr
               (T.sprintf [T.red; T.Bold]
                  "  An error occurred while writing %s: %s"
                  (Filename.concat path name)
                  (Printexc.to_string ex))))

  let dump s =
    let rec iter i_d =
      if i_d < (Hashtbl.length s.h) then begin
        Hashtbl.find s.h i_d
        |> (fun b ->
            let name = string_of_int i_d in
            _async Misc.write_json b (name ^ ".json") s.path;
            (* _async Big.write_dot b (name ^ ".dot") s.path; *)
            _async Misc.write_svg b (name ^ ".svg") s.path;
            iter (i_d + 1))
      end else { s with last_dump = Some (i_d - 1); } in
    iter @@ (match s.last_dump with
        | None -> 0
        | Some i -> i + 1)

end

let log_msg_f s msg addr =
  T.sprintf s "%s (%s)" msg (Misc.string_of_addr addr)

let log_msg = log_msg_f [T.green]
let log_msg_w = log_msg_f [T.yellow]
let log_msg_e = log_msg_f [T.red; T.Bold]

let log_lines l =
  List.map (fun s -> T.sprintf [T.green] "%s" s) l
  |> String.concat "\n"
  |> Lwt_log.info

let telemetry event time_received millis processing_time size id =
  Lwt_log.debug_f "%s,%s,%f,%d,%d"
    event (Misc.timestamp_short time_received millis) processing_time size id

let func_telemetry func_time =
  Lwt_log.debug_f "%s" func_time

(*  Applies the event to the bigraphical model. *)
let apply_rules state descr_e addr (rules, g_rules, preds) =
  let starting_time = Unix.gettimeofday () in
  let millis = (int_of_float ((mod_float starting_time 1.0) *. 1000.0)) in
  let arrival_time = Unix.localtime @@ starting_time in
  (if descr_e = "reading depleted" then log_lines [descr_e]
   else log_lines ["Received " ^ descr_e]) >>= fun () ->
  Lwt_mvar.take state >>= fun s -> (* take the lock *)
  log_lines ["Processing " ^ descr_e] >>= fun () ->
  Lwt.catch
    (fun () ->
       let current_method_start = Unix.gettimeofday () in
       let (b, index) = State.current s in
       let current_method_exec_time = (Unix.gettimeofday () -. current_method_start) *. 1000.0 in
       func_telemetry ("State.current->" ^ (string_of_float current_method_exec_time)) >>= fun () ->
       (* Apply rules in sequence without storing intermediate steps *)
       let apply_method_start = Unix.gettimeofday () in
       Brs.apply b rules
       |> (function
           | None -> Lwt.fail_with ("Reaction rules for "
                                    ^ descr_e
                                    ^ " cannot be applied")
           | Some b ->
             begin
               let apply_method_exec_time = (Unix.gettimeofday () -. apply_method_start) *. 1000.0 in
               func_telemetry  ("Brs.apply->" ^ (string_of_float apply_method_exec_time)) >>= fun () ->
               let fix_method_start = Unix.gettimeofday () in
               let b'=
                 match g_rules with
                 | [] -> b
                 | _ -> fst @@ Brs.fix b g_rules in
               let fix_method_exec_time = (Unix.gettimeofday () -. fix_method_start) *. 1000.0 in
               func_telemetry  ("Brs.fix->" ^ (string_of_float fix_method_exec_time)) >>= fun () ->
               let register_method_start = Unix.gettimeofday () in
               State.register preds s;
               let register_method_exec_time = (Unix.gettimeofday () -. register_method_start) *. 1000.0 in
               func_telemetry ("State.register->" ^ (string_of_float register_method_exec_time)) >>= fun () ->
               let update_method_start = Unix.gettimeofday () in
               (State.update b' s
                |> Lwt_mvar.put state) >>= fun () -> (* release the lock *)
               let update_method_exec_time = (Unix.gettimeofday () -. update_method_start) *. 1000.0 in
               func_telemetry ("State.update->" ^ (string_of_float  update_method_exec_time)) >>= fun () ->
               let proc_time =  (Unix.gettimeofday () -. starting_time) *. 1000.0
               and size = Big.size b in
               telemetry descr_e arrival_time millis proc_time size index >>= fun () ->
               log_lines [ descr_e ^ ": OK";
                           "State id: " ^ (string_of_int (index + 1));
                           "Rule processing time: "
                           ^ string_of_float (proc_time)
                           ^ "ms" ]  >>= fun () ->
               return (descr_e ^ ": OK")
             end))

    (fun ex ->
       Lwt_mvar.put state s >>= fun () -> (* release the lock *)
       begin
         match ex with
         | Failure msg ->
           Lwt_log.error @@ log_msg_e msg addr
         | ex ->
           Lwt_log.error (log_msg_e ("BigraphER internal error: "
                                     ^ (Printexc.to_string ex) ^ ": "
                                     ^ "Unable to process "
                                     ^ descr_e) addr)
       end >>= fun () ->
       return "BigraphER internal error")

(* The global state is updated according to the event received *)
let handle_message state msg addr =
  let print_header label addr =
    Lwt_log.info ("\n\n" ^ (log_msg ("-------------------- event -> " ^ label
                                     ^ " -------- ") addr))
  and print_event label prm =
    let descr = descr_of_arg label in
    log_lines ["Event: " ^ label ^ "\t" ^ descr ^ ": " ^ prm]
  and print_event_binary label prm1 prm2 =
    let (descr1, descr2) = descr_of_args label in
    log_lines ["Event: " ^ label ^ "\t"
               ^ descr1 ^ ": " ^ prm1 ^ "\t"
               ^ descr2 ^ ": " ^ prm2] in
  match Misc.parse_message ~msg ~delim:"->" with
  (* Events with one argument *)
  | label :: prm :: [] ->
    begin
      print_header label addr >>= fun () ->
      print_event label prm >>= fun () ->
      match parse_event ~label ~prm with
      | Some e -> rules_of_event e |> apply_rules state (descr e) addr
      | None ->
        Lwt_log.warning (log_msg_w "Unknown event" addr) >>= fun () ->
        return "Unknown event"
    end
    (* Events with two arguments *)
  | label :: prm1 :: prm2 :: [] ->
    begin
      print_header label addr >>= fun () ->
      print_event_binary label prm1 prm2 >>= fun () ->
      try
        match parse_event_binary ~label ~prm1 ~prm2 with
        | Some e -> rules_of_event e |> apply_rules state (descr e) addr
        | None ->
          Lwt_log.warning (log_msg_w "Unknown event" addr) >>= fun () ->
          return "Unknown event"
      with
      (* Triggered by malformed reading events *)
      | Failure _ | Invalid_argument _ ->
        Lwt_log.warning (log_msg_w "Malformed payload" addr) >>= fun () ->
        return "Malformed payload"
    end
  | _ ->
    Lwt_log.warning (log_msg_w "Malformed event" addr) >>= fun () ->
    return "Malformed event"

(* Reads and processes a message from input stream. *)
let rec handle_connection state ic oc addr () =
  Lwt_io.read_line_opt ic >>=
  (fun msg ->
     match msg with
     | Some msg ->
       handle_message state msg addr
       >>= Lwt_io.write_line oc
       >>= handle_connection state ic oc addr
     | None ->
       Lwt_log.info (log_msg "Connection closed" addr) >>= return)

(* Sets up input/output channels for a new socket connection. *)
let accept_connection state (fd, addr) =
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd
  and oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  Lwt.on_failure
    (handle_connection state ic oc addr ())
    (fun e -> Lwt_log.ign_error (Printexc.to_string e));
  Lwt_log.info (log_msg "New connection" addr) >>= return

(* Creates a serves-side socket connection and processes incoming messages. *)
let create_server s sock host port size =
  let open Lwt_unix in
  bind sock @@ ADDR_INET (host, port) >>= fun () ->
  listen sock size;
  let rec serve () =
    accept sock >>= accept_connection @@ s >>= serve in
  serve ()

(* Aquire the lock, update the state by executing f, and release the lock. *)
let access_state state (f: State.t -> State.t t) =
  Lwt_mvar.take state >>= fun s ->
  f s >>= fun s' ->
  Lwt_mvar.put state s'

(* Dump new states *)
let dump_f s ~mute =
  let (_, last_i) = State.current s in
  let print_f a s =
    Lwt_io.write_line Lwt_io.stdout
      ("  Dumping states ["
       ^ (string_of_int a) ^ ","
       ^ (string_of_int last_i) ^ "] to "
       ^ State.(s.path)) >>= fun () ->
    return @@ State.dump s in
  match State.(s.last_dump) with
  | Some i ->
    if i = last_i then if mute then return s
      else Lwt_io.write_line Lwt_io.stdout "  Nothing to dump" >>= fun () ->
        return s
    else print_f i s
  | None -> print_f 0 s

(* Runs the commands if in the following list. *)
let process_cmd state = function
  | "help" -> Lwt_io.write_line Lwt_io.stdout
                "COMMANDS:\n\
                \  dump\t\t\tSave current trace to file\n\
                \  help\t\t\tShow this help\n\
                \  print_state\t\tPrint the current state\n\
                \  stats\t\t\tShow stats"
  | "stats" -> access_state state (fun s ->
      Lwt_io.write_line Lwt_io.stdout
        (State.string_of s) >>= fun () ->
      return s)
  | "dump" -> access_state state @@ dump_f ~mute:false
  | "print_state" -> access_state state (fun s ->
      let (b, index) = State.current s in
      Lwt_io.write_line Lwt_io.stdout
        ("Current state: "
         ^ (string_of_int index) ^ "\nPredicates: "
         ^ (Misc.string_of_ints @@ State.label s index) ^ "\n"
         ^ (Big.to_string b)) >>= fun () ->
      return s)
  | _ -> Lwt_io.write_line Lwt_io.stdout "Unknown command"

(* Takes console input and runs the command. *)
let rec terminal state () =
  Lwt_io.read_line_opt Lwt_io.stdin >>= function
  | Some cmd ->
    (Lwt.catch
       (fun () -> process_cmd state cmd)
       (function
         | ex ->
           Lwt_log.error (T.sprintf [T.red] "%s" (Printexc.to_string ex))))
    >>= terminal @@ state
  | None -> return_unit

(* Dump process *)
let rec dumpd state time () =
  Lwt_unix.sleep time >>= fun () ->
  access_state state @@ dump_f ~mute:true >>= dumpd state time

let print_config path_arg dump_arg size_arg tele_arg host_arg port_arg =
  let banner = T.sprintf [T.cyan]
      "      __    _       __ __                         %s (2017, 2018)
     / /_  (_)___ _/ // /_      ___________
    / __ \\/ / __ `/ // /| | /| / / ___/ __ \\            Milan Kabac
   / /_/ / / /_/ /__  __/ |/ |/ (__  ) / / /      Michele Sevegnani
  /_.___/_/\\__, /  /_/  |__/|__/____/_/ /_/
          /____/              http://www.dcs.gla.ac.uk/research/S4/"
      version in
  print_endline
    (banner
     ^ "\n\nCONFIGURATION:\n  Dump path:\t\t" ^ path_arg
     ^ "\n  Dump timeout:\t\t"
     ^ (match dump_arg with
         | None -> "-"
         | Some t -> (string_of_float t))
     ^ "\n  Record telemetry:\t"
     ^ (match tele_arg with
         | None -> "-"
         | Some f -> f)
     ^ "\n  Server address:\t" ^ (Unix.string_of_inet_addr host_arg)
     ^ ":" ^ (string_of_int port_arg)
     ^ "\n  Socket size:\t\t" ^ (string_of_int size_arg))

let server_cmd path_arg dump_arg size_arg tele_arg host_arg port_arg =
  (* Print configuration *)
  print_config path_arg dump_arg size_arg tele_arg host_arg port_arg;
  let state =
    Misc.mkdir path_arg;
    State.init path_arg
    |> State.update initial_state
    |> Lwt_mvar.create
  and sock = Lwt_unix.socket PF_INET SOCK_STREAM 0
  and init_log =
    (match tele_arg with
     | Some f ->
       Lwt_log.file
         ~template:"$(message)"
         ~file_name:(Filename.concat path_arg f)
         ()
     | None -> return Lwt_log.null) >>= fun logger ->
    (Lwt_log_core.default :=
       Lwt_log.dispatch (fun _ level ->
           match level with
           | Lwt_log_core.Debug -> logger
           | Lwt_log_core.Info | Lwt_log_core.Notice | Lwt_log_core.Warning
           | Lwt_log_core.Error | Lwt_log_core.Fatal ->
             Lwt_log.channel
               ~template:"[$(date)] $(message)"
               ~close_mode:`Keep
               ~channel:Lwt_io.stdout
               ());
     Lwt_log.add_rule "*" Lwt_log.Info;
     Lwt_log.add_rule "*" Lwt_log.Debug;
     return_unit) in
  begin match dump_arg with
    | Some t -> Lwt.async @@ dumpd state t;
    | None -> ()
  end;
  Lwt_main.run
    (terminal state ()
     <?>
     (init_log >>= fun () ->
      create_server state sock host_arg port_arg size_arg))

(* Command synopsis:
   big4wsn_server [OPTION...] [HOST [PORT]]

   Options:

   -d, --dump=TIMEOUT
   -p, --path=PATH
   -s, --size=SOCKET_SIZE
   -t, --telemetry=LOG_FILE

   -?, --help                 give this help list
   -V, --version              print program version *)

open Cmdliner

let path_arg =
  let doc = "Save new states to $(docv)." in
  Arg.(value & opt string Defaults.path
       & info ["p"; "path"] ~docv:"PATH" ~doc)

let dump_arg =
  let doc = "Automatically save new states each $(docv) seconds." in
  Arg.(value & opt ~vopt:(Some Defaults.t_dumpd) (some float) None
       & info ["d"; "dump"] ~docv:"TIMEOUT" ~doc)

let size_arg =
  let doc = "Use a socket of size $(docv)." in
  Arg.(value & opt int Defaults.backlog
       & info ["s"; "size"] ~docv:"SOCKET_SIZE" ~doc)

let tele_arg =
  let doc = "Save telemetry to $(docv). Default save location is "
            ^ Defaults.path
            ^ " which is overridden by option $(b,-p) $(b,--path) when present." in
  Arg.(value & opt ~vopt:(Some Defaults.csv_log) (some string) None
       & info ["t"; "telemetry"] ~docv:"LOG_FILE" ~doc)

let host_arg =
  let u_addr =
    let parse s =
      try Ok (Unix.inet_addr_of_string s) with
      | Failure _ -> Error (`Msg "unable to parse host IP address") in
    let print ppf addr = Format.fprintf ppf "%s" (Unix.string_of_inet_addr addr) in
    Arg.conv (parse, print) in
  let doc = "Set server IP address to $(docv)" in
  Arg.(value & pos 0 u_addr Defaults.listen_address & info [] ~docv:"HOST" ~doc)

let port_arg =
  let doc = "Set server port to $(docv)" in
  Arg.(value & pos 1 int Defaults.port & info [] ~docv:"PORT" ~doc)

let server_t =
  Term.(const server_cmd
        $ path_arg $ dump_arg $ size_arg $ tele_arg $ host_arg $ port_arg)

let info_t =
  let doc = "encode network events into \
             bigraphs for formal analysis." in
  let man = [
    `S Manpage.s_bugs;
    `P "Email bug reports to <michele.sevegnani@glasgow.ac.uk>." ] in
  Term.info "big4wsn_server" ~version:version ~doc ~exits:Term.default_exits ~man

let () = Term.(exit @@ eval (server_t, info_t))

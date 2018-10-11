open Lwt

let send_events sock addr events =
  let handler oc events () =
    Lwt_list.iter_s (fun e ->
      match e with
      | "read" | "inc" ->
        Lwt_io.write_line oc e >>= fun () ->
        Lwt_log.info ("Event \"" ^ e ^ "\" sent")
      | _ -> Lwt_log.info "Unknown event") events in
  Lwt_unix.connect sock addr >>= fun () ->
  Lwt_log.info "Connected to socket" >>= fun () ->
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output sock in
  Lwt.on_failure
    (Lwt_log.info ("Sending events to " ^ (Misc.string_of_addr addr)) >>= fun () ->
     handler oc events ())
    (fun e -> Lwt_log.ign_error (Printexc.to_string e));
  Lwt_log.info "Events sent" >>= return

(* Events are comma separated strings: "read" and "inc". *)
let client () =
  if Array.length Sys.argv != 4 then begin
    prerr_endline "Usage: client <host> <port> <events>";
    exit 2;
  end;
  let server_name = Sys.argv.(1)
  and port_number = int_of_string Sys.argv.(2)
  and events =
    String.trim Sys.argv.(3)
    |> String.split_on_char ','
    |> List.map String.trim in
  Lwt_log.info ("Events: [" ^ (String.concat "," events)
                ^ "]\nLooking up host: " ^ server_name) >>= fun () ->
  Lwt_unix.gethostbyname server_name >>= fun h ->
  let server_addr = h.Lwt_unix.h_addr_list.(0) in
  let sock = Lwt_unix.socket h.Lwt_unix.h_addrtype Lwt_unix.SOCK_STREAM 0 in
  Lwt_log.info "Socket created" >>= fun () ->
  send_events sock (Lwt_unix.ADDR_INET (server_addr, port_number)) events 

let () =
  Lwt_log.add_rule "*" Lwt_log.Info;
  Lwt_main.run @@ Lwt_unix.handle_unix_error client ()

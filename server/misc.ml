(* Utility functions *)

let string_of_addr = function
  | Lwt_unix.ADDR_UNIX s -> s
  | Lwt_unix.ADDR_INET (a, p) ->
    (Unix.string_of_inet_addr a) ^ ":" ^ (string_of_int p)

(* 14:41:28 UTC *)
let timestamp_short t ms =
  let open Unix in
  (string_of_int t.tm_hour) ^ ":"
  ^ (string_of_int t.tm_min) ^ ":"
  ^ (string_of_int t.tm_sec) ^ "."
  ^ (string_of_int ms)

(* 2017-02-28 14:41:28 UTC *)
let timestamp t =
  let open Unix in
  let tm = gmtime t in
  (string_of_int (1900 + tm.tm_year)) ^ "-"
  ^ (string_of_int tm.tm_mon) ^ "-"
  ^ (string_of_int tm.tm_mday) ^ " "
  ^ (string_of_int tm.tm_hour) ^ ":"
  ^ (string_of_int tm.tm_min) ^ ":"
  ^ (string_of_int tm.tm_sec) ^ " UTC"

(* 2017-02-28-big4wsn_server *)
let path t =
  let open Unix in
  let tm = gmtime t in
  (string_of_int (1900 + tm.tm_year)) ^ "-"
  ^ (string_of_int tm.tm_mon) ^ "-"
  ^ (string_of_int tm.tm_mday) ^ "-big4wsn_server"

let mkdir s =
  let open Unix in
  try mkdir s 0o740 with
  | Unix_error (EEXIST, _, _) -> ()

let print_list =
  List.iter print_endline

(* parse_message "reading->0,2017-02-28 14:41:28 UTC,3,23.8,24.06,23.82,987.54,0.16,0.0,99.84,100.0";; *)
let parse_message ~msg ~delim =
  Str.split (Str.regexp_string delim) msg

let float_at_id i l =
  float_of_string (List.nth l i)

(* Return a list fo lenght n with all elemnts equal to x *)
let rec init_list x n acc =
  if n = 0 then acc
  else init_list x (n - 1) (x::acc)

let string_of_list f l =
  List.map f l
  |> String.concat ","
  |> (fun s -> "[" ^ s ^ "]")

let string_of_ints =
  string_of_list string_of_int

(* Export functions *)
type error =
  | Dot_not_found
  | Dot_stopped of int
  | Dot_killed of int
  | Dot_internal_error of int
  | Internal_error of Unix.error * string * string
  | Sys of string

exception ERROR of error

let report_error = function
  | Dot_not_found -> "`dot' command not found"
  | Dot_stopped i -> "`dot' stopped by signal " ^ (string_of_int i)
  | Dot_killed i -> "`dot' killed by signal " ^ (string_of_int i)
  | Dot_internal_error i -> "`dot' returned with code " ^ (string_of_int i)
  | Sys s -> s
  | Internal_error (e, fname, arg) ->
    (Unix.error_message e) ^ " at \""^ fname ^ "\" \"" ^ arg ^ "\""

let rec restart_on_EINTR f x =
  try f x with Unix.Unix_error (Unix.EINTR, _, _) -> restart_on_EINTR f x

let _write_svg s name path =
  let (dot_in, bigmc_out) = Unix.pipe ()
  and n_path = Filename.concat path name in
  match Unix.fork () with
  | 0 ->
    (* child *)
    (try
       Unix.close bigmc_out;
       Unix.dup2 dot_in Unix.stdin;
       Unix.close dot_in;
       let svg_file =
         Unix.openfile
           n_path [ Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY ] 0o600 in
       Unix.dup2 svg_file Unix.stdout;
       Unix.close svg_file;
       Unix.execvp "dot" [| "dot"; "-Tsvg" |]
     with
     | _ -> exit 127)
  | pid ->
    (* parent *)
    (Unix.close dot_in;
     let b_w = Unix.write_substring bigmc_out s 0 (String.length s) in
     Unix.close bigmc_out;
     match snd (restart_on_EINTR (Unix.waitpid []) pid) with
     | Unix.WSTOPPED i -> raise (ERROR (Dot_stopped i))
     | Unix.WSIGNALED i -> raise (ERROR (Dot_killed i))
     | Unix.WEXITED 0 -> b_w
     | Unix.WEXITED 127 -> raise (ERROR Dot_not_found)
     | Unix.WEXITED i -> raise (ERROR (Dot_internal_error i)))

let catch_unix_errors f arg name path =
  try f arg name path with
  | Unix.Unix_error (e, fname, args) ->
    raise (ERROR (Internal_error (e, fname, args)))

let write_svg s ~name ~path =
  catch_unix_errors _write_svg s name path

let write_string s ~name ~path =
  try
    let out_ch =
      Filename.concat path name
      |> open_out in
    output_string out_ch s;
    close_out out_ch;
    String.length s
  with
  | Sys_error s -> raise (ERROR (Sys s))

let write_svg b ~name ~path =
  try write_svg (Bigraph.Big.to_dot b name) ~name ~path with
  | ERROR e -> failwith @@ report_error e

let write_json b ~name ~path =
  try (Big_json.big_to_json ~minify:false b
       |> write_string ~name ~path) with
  | ERROR e -> failwith @@ report_error e 

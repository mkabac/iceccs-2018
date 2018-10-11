(* Module for the conversion of network events to bigraphical reaction rules *)

(* Mapped to the position of a CSV reading *)

open Misc

type sensor_data =
  { date : string;
    temp : float;
    humidity : float;
    light : float;
    pressure : float;
    e_consumed : float;
    e_gen : float;
    battery : float;
    battery_max : float;
  }

let ex_data =
  { date = "2017-02-28 14:41:28 UTC";
    temp = 23.8;
    humidity = 24.06;
    light = 23.82;
    pressure = 987.54;
    e_consumed = 0.16;
    e_gen = 0.0;
    battery = 99.84;
    battery_max = 100.0;
  }

(* The type of events *)
type event =
  | New_node of string * string
  | New_link of string * string
  | Drop_link of string * string
  | Open_link of string * string
  | Close_link of string * string
  | Node_failure of string
  | Node_recover of string
  | Node_sensor_reading of string * sensor_data
  | Node_sensor_reading_depleted of string * sensor_data
  | Join of string * string
  | Leave of string
  | Move of string * string
  | Deploy_app of string * int
  | Deploy_on_node of int * string
  (* .... *)

(* Description of events *)
let descr = function
  | New_link (_, _) -> "new link"
  | Drop_link (_, _) -> "drop link"
  | Open_link (_, _) -> "open link"
  | Close_link (_, _) -> "close link"
  | New_node (_, _) -> "new node"
  | Node_failure _ -> "node failure"
  | Node_recover _ -> "node recover"
  | Node_sensor_reading (_, _) -> "reading"
  | Node_sensor_reading_depleted (_, _) -> "reading depleted"
  | Join (_, _) -> "join"
  | Leave _ -> "leave"
  | Move (_, _) -> "move"
  | Deploy_app (_, _) -> "deploy app"
  | Deploy_on_node (_, _) -> "deploy on node"
  (* .... *)

(* Create a func that splits a composite string into key and value.
   use key to match message in parse_event and use value as the second parameter
   in the rule.*)

(* Just for testing. We will use this format later on
   Example:
   "event->ip->Date,Entry ID,Temperature,Humidity,Light Level,Atmospheric Pressure,Energy Consumed,Energy Generated,Battery State,Max Battery"
   "reading->2001:0db8:0000:0000:0000:ff00:0042:8329->2017-02-28 14:41:28 UTC,3,23.8,24.06,23.82,987.54,0.16,0.0,99.84,100.0"
*)

let str_to_data s =
  let reading_list = parse_message ~msg:s ~delim:"," in
  (* print_list reading_list; *)
  { (* date = List.nth reading_list 1; *)
    date = "2017-02-28 14:41:28 UTC";
    temp = float_at_id 1 reading_list;
    humidity = float_at_id 2 reading_list;
    light = float_at_id 3 reading_list;
    pressure = float_at_id 4 reading_list;
    e_consumed = float_at_id 5 reading_list;
    e_gen = float_at_id 6 reading_list;
    battery = float_at_id 7 reading_list;
    battery_max = float_at_id 8 reading_list;
  }

(* Events with one parameter *)
let parse_event ~label ~prm =
  match label with
  | "leave" -> Some (Leave prm)
  | "failure_node" -> Some (Node_failure prm)
  | "recover_node" -> Some (Node_recover prm)
  | _ -> None

(* Events with two parameters *)
let parse_event_binary ~label ~prm1 ~prm2 =
  match label with
  | "new_node" -> Some (New_node (prm1, prm2)) (* MAC, location => Done *)
  | "new_link" -> Some (New_link (prm1, prm2)) (* IPv6, IPv6 => Done *)
  | "drop_link" -> Some (Drop_link (prm1, prm2))
  | "open_link" -> Some (Open_link (prm1, prm2))
  | "close_link" -> Some (Close_link (prm1, prm2))
  | "join" -> Some (Join (prm1, prm2)) (* MAC, IPv6  => Done *)
  | "reading" -> (* IPv6, data => Done *)
    begin
        let r = str_to_data prm2 in
        if (compare r.battery 0.0) = 0 then
          Some (Node_sensor_reading_depleted (prm1, r))
        else Some (Node_sensor_reading (prm1, r))
    end
  | "move" -> Some (Move (prm1, "Bridge"))
  | "deploy" -> Some (Deploy_app (prm1, int_of_string prm2)) (* App name, AppID => Done *)
  | "deploy_on" -> Some (Deploy_on_node (int_of_string prm1, prm2)) (* AppID, IPnode => Done *)
  | _ -> None

(* Return: 
   - a list of rules to be applied in sequence and
   - a list of rules to be applied until a fixed point is reached
   - a list of predicates *)
let rules_of_event = function
  | New_node (addr, loc) ->
    ([ Model.new_node addr loc ], [], [])
  | New_link (addr1, addr2) ->
    ([ Model.new_link addr1 addr2;
       Model.used_new_link addr1 addr2;
       Model.used_new_link addr2 addr1;
       Model.used_used_new_link addr1 addr2], [], [])
  | Drop_link (addr1, addr2) ->
    ([ Model.drop_link addr1 addr2;
       Model.used_drop_link addr1 addr2;
       Model.used_drop_link addr2 addr1;
       Model.used_used_drop_link addr1 addr2;], [], [])
  | Open_link (addr, out) ->
    ([ Model.drop_link addr out ], [], []) (* TO BE FIXED *)
  | Close_link (addr, out) ->
    ([ Model.drop_link addr out ], [], []) (* TO BE FIXED *)
  | Node_failure addr -> (* All links with the node are lost *)
    ([ Model.node_fail addr; Model.used_node_fail addr ],
     [ Model.broken_link ], [])
  | Node_recover addr -> (* Node needs to rejoin the network afterwards *)
    ([ Model.node_recover addr ],
     [], [])
  | Join (mac, addr) ->
    ([ Model.join mac addr ], [], [])
  | Leave addr -> (* All links with the node are lost *)
    ([ Model.leave addr; Model.used_leave addr ],
     [ Model.broken_link ], [])
  | Node_sensor_reading (addr, r) ->
    ([ Model.reading
         addr
         r.date
         r.temp
         r.humidity
         r.light
         r.pressure
         r.e_consumed
         r.e_gen
         r.battery
         r.battery_max ],
     [], [])
  | Node_sensor_reading_depleted (addr, r) ->
    ([ Model.reading_depleted
         addr
         r.date
         r.temp
         r.humidity
         r.light
         r.pressure
         r.e_consumed
         r.e_gen
         r.battery_max ],
     [], [])
  | Deploy_app (n, id) ->
    ([ Model.deploy n id], [], []) (* Add predicates to model app's requirements *)
  | Deploy_on_node (id, addr) ->
    ([ Model.deploy_on_used_node id addr;
       Model.deploy_on_node id addr ],
     [], [])
  | Move (addr, loc) ->
    ([ Model.move addr loc ], [], [])
(* .... *)

(* The initial state is a bigraph representing an empty network with only a link
   to the outside world. *)
let initial_state = Model.s0 "out"

let descr_of_arg = function
  | "failure_node" -> "IP"
  | "recover_node" -> "IP"
  | "leave" -> "IP"
  | _ -> "value"

let descr_of_args = function
  | "reading" -> ("IP", "Payload")
  | "new_node" -> ("MAC", "Location")
  | "join" -> ("MAC", "IP")
  | "new_link"
  | "drop_link"
  | "close_link"
  | "open_link" -> ("IP", "IP")
  | "deploy" -> ("Name", "AppID")
  | "deploy_on" -> ("AppID", "IP")
  | "move" -> ("IP", "Location")
  | _ -> ("value1", "value2")

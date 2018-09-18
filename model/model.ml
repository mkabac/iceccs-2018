(* model.big *)
open Bigraph

let ctrl_Int (x:int) =
  Ctrl.C ("Int", [ Ctrl.I x ], 0)

let ctrl_Float (x:float) =
  Ctrl.C ("Float", [ Ctrl.F x ], 0)

let ctrl_String (x:string) =
  Ctrl.C ("String", [ Ctrl.S x ], 0)

let ctrl_N = Ctrl.C ("N", [], 1)

let ctrl_N_U = Ctrl.C ("N_U", [], 1)

let ctrl_N_F = Ctrl.C ("N_F", [], 1)

let ctrl_L = Ctrl.C ("L", [], 0)

let ctrl_L_E = Ctrl.C ("L_E", [], 1)

let ctrl_App (x:string) =
  Ctrl.C ("App", [ Ctrl.S x ], 0)

let ctrl_A (x:int) =
  Ctrl.C ("A", [ Ctrl.I x ], 0)

let ctrl_Conf = Ctrl.C ("Conf", [], 1)

let ctrl_MAC (x:string) =
  Ctrl.C ("MAC", [Ctrl.S x ], 0)

let ctrl_IPv6 (x:string) =
  Ctrl.C ("IPv6", [Ctrl.S x ], 0)

let ctrl_IPv6_unassigned = Ctrl.C ("IPv6_unassigned", [], 0)

let ctrl_Sensors = Ctrl.C ("Sensors", [], 0)

let ctrl_Date (x:string) =
  Ctrl.C ("Date", [Ctrl.S x ], 0)

let ctrl_Temperature (x:float) =
  Ctrl.C ("Temperature", [ Ctrl.F x ], 0)

let ctrl_Humidity (x:float) =
  Ctrl.C ("Humidity", [ Ctrl.F x ], 0)

let ctrl_Light_level (x:float) =
  Ctrl.C ("Light_level", [ Ctrl.F x ], 0)

let ctrl_Pressure (x:float) =
  Ctrl.C ("Pressure", [ Ctrl.F x ], 0)

let ctrl_Energy_consumed (x:float) =
  Ctrl.C ("Energy_consumed", [ Ctrl.F x ], 0)

let ctrl_Energy_generated (x:float) =
  Ctrl.C ("Energy_generated", [ Ctrl.F x ], 0)

let ctrl_Battery_state (x:float) =
  Ctrl.C ("Battery_state", [ Ctrl.F x ], 0)

let ctrl_Battery_depleted = Ctrl.C ("Battery_depleted", [], 0)

let ctrl_Max_battery (x:float) =
  Ctrl.C ("Max_battery", [ Ctrl.F x ], 0)

let ctrl_Park = Ctrl.C ("Park", [], 0)

let ctrl_Bridge = Ctrl.C ("bridge", [], 0)

let ctrl_Hospital = Ctrl.C ("Hospital", [], 0)

let ctrl_North = Ctrl.C ("North", [], 0)

let ctrl_South = Ctrl.C ("South", [], 0)

let ctrl_PHY = Ctrl.C ("PHY", [], 0)

let ctrl_DATA = Ctrl.C ("DATA", [], 0)

let ctrl_CONF = Ctrl.C ("CONF", [], 0)

let id1 =
  Big.id (Big.Inter (1, Link.parse_face []))

let n_c =
  Link.parse_face ["c"]

let n_l =
  Link.parse_face ["l"]

(* hostname models a channel to the ouside world *)
let s0 (hostname:string) =
  Big.ppar
    (Big.ppar
       (Big.par
          (Big.nest
             (Big.ion (Link.parse_face []) ctrl_PHY)
             (Big.par
                (Big.nest
                   (Big.ion (Link.parse_face []) ctrl_North)
                   (Big.par
                      (Big.par
                         (Big.nest
                            (Big.ion (Link.parse_face []) ctrl_Hospital)
                            Big.one)
                         (Big.nest
                            (Big.ion (Link.parse_face []) ctrl_Bridge)
                            Big.one))
                      (Big.nest
                         (Big.ion (Link.parse_face []) ctrl_Park)
                         Big.one)))
                (Big.nest
                   (Big.ion (Link.parse_face []) ctrl_South)
                   Big.one)))
          (Big.intro (Link.Face.singleton (Link.Name hostname))))
       (Big.nest
          (Big.ion (Link.parse_face []) ctrl_DATA)
          Big.one))
    (Big.nest
       (Big.ion (Link.parse_face []) ctrl_CONF)
       Big.one)

let _node_fail ctrl_node ipv6 =
  Brs.parse_react_unsafe
    ~lhs:(Big.close
            n_c
            (Big.ppar
               (Big.ion n_c ctrl_node)
               (Big.nest
                  (Big.ion n_c ctrl_Conf)
                  (Big.par
                     (Big.atom Link.Face.empty (ctrl_IPv6 ipv6))
                     id1))))
    ~rhs:(Big.close
            n_c
            (Big.ppar
               (Big.atom n_c ctrl_N_F)
               (Big.nest
                  (Big.ion n_c ctrl_Conf)
                  (Big.par
                     (Big.atom Link.Face.empty (ctrl_IPv6 ipv6))
                     id1))))
        (Some (Fun.parse [1]))

let node_fail = _node_fail ctrl_N
    
let used_node_fail = _node_fail ctrl_N_U

let node_recover ipv6 =
  Brs.parse_react_unsafe
    ~lhs:(Big.close
            n_c
            (Big.ppar
               (Big.atom n_c ctrl_N_F)
               (Big.nest
                  (Big.ion n_c ctrl_Conf)
                  (Big.par
                     (Big.atom Link.Face.empty (ctrl_IPv6 ipv6))
                     id1))))
    ~rhs:(Big.close
            n_c
            (Big.ppar
               (Big.nest
                  (Big.ion n_c ctrl_N)
                  (Big.nest
                     (Big.ion Link.Face.empty ctrl_L)
                     Big.one))
               (Big.nest
                  (Big.ion n_c ctrl_Conf)
                  (Big.par
                     (Big.atom Link.Face.empty ctrl_IPv6_unassigned)
                     id1))))
    None

let conf mac =
  Big.par
    (Big.par
       (Big.atom Link.Face.empty (ctrl_MAC mac))
       (Big.atom Link.Face.empty ctrl_IPv6_unassigned))
    (Big.nest
       (Big.ion Link.Face.empty ctrl_Sensors)
       Big.one)

(* Assume exisiting unique loc id *)
let new_node mac loc =
  Brs.parse_react_unsafe
    ~lhs:(Big.ppar
            (Big.ion Link.Face.empty (Ctrl.C (loc, [], 0)))
            (Big.ion Link.Face.empty ctrl_DATA))
    ~rhs:(Big.close n_c
            (Big.ppar
               (Big.nest
                  (Big.ion Link.Face.empty (Ctrl.C (loc, [], 0)))
                  (Big.par
                     (Big.nest
                        (Big.ion n_c ctrl_N)
                        (Big.nest
                           (Big.ion Link.Face.empty ctrl_L)
                           Big.one))
                     id1))
               (Big.nest
                  (Big.ion Link.Face.empty ctrl_DATA)
                  (Big.par
                     (Big.nest
                        (Big.ion n_c ctrl_Conf)
                        (conf mac))
                     id1))))
    None

let join mac ipv6 =
  Brs.parse_react_unsafe
    ~lhs:(Big.close
            n_c
            (Big.ppar
               (Big.ion n_c ctrl_N)
               (Big.nest
                  (Big.ion n_c ctrl_Conf)
                  (Big.par
                     (Big.par
                        (Big.atom Link.Face.empty (ctrl_MAC mac))
                        (Big.atom Link.Face.empty ctrl_IPv6_unassigned))
                     id1))))
    ~rhs:(Big.close
            n_c
            (Big.ppar
               (Big.ion n_c ctrl_N)
               (Big.nest
                  (Big.ion n_c ctrl_Conf)
                  (Big.par
                     (Big.par
                        (Big.atom Link.Face.empty (ctrl_MAC mac))
                        (Big.atom Link.Face.empty (ctrl_IPv6 ipv6)))
                     id1))))
    None

let _leave ipv6 node =
  Brs.parse_react_unsafe
    ~lhs:(Big.close n_c
            (Big.ppar
               node
               (Big.nest
                  (Big.ion n_c ctrl_Conf)
                  (Big.par
                     (Big.atom Link.Face.empty (ctrl_IPv6 ipv6))
                     id1))))
    ~rhs:(Big.close n_c
            (Big.ppar
               (Big.nest
                  (Big.ion n_c ctrl_N)
                  (Big.nest
                     (Big.ion Link.Face.empty ctrl_L)
                     Big.one))
               (Big.nest
                  (Big.ion n_c ctrl_Conf)
                  (Big.par
                     (Big.atom Link.Face.empty ctrl_IPv6_unassigned)
                     id1))))
    (Some (Fun.parse [1]))


let leave ipv6 =
  _leave ipv6 (Big.ion n_c ctrl_N)

let used_leave ipv6 =
  _leave ipv6 (Big.ion n_c ctrl_N_U)

let _new_link node1 node2 addr1 addr2 =
  Brs.parse_react_unsafe
    ~lhs:(Big.close
            (Link.parse_face ["c1"; "c0"])
            (Big.ppar
               (Big.ppar
                  (Big.nest
                     (Big.ion (Link.parse_face ["c0"]) node1)
                     (Big.par
                        (Big.ion Link.Face.empty ctrl_L)
                        id1))
                  (Big.nest
                     (Big.ion (Link.parse_face ["c1"]) node2)
                     (Big.par
                        (Big.ion Link.Face.empty ctrl_L)
                        id1)))
               (Big.par
                  (Big.nest
                     (Big.ion (Link.parse_face ["c0"]) ctrl_Conf)
                     (Big.par
                        (Big.atom Link.Face.empty (ctrl_IPv6 addr1))
                        id1))
                  (Big.nest
                     (Big.ion (Link.parse_face ["c1"]) ctrl_Conf)
                     (Big.par
                        (Big.atom Link.Face.empty (ctrl_IPv6 addr2))
                        id1)))))
    ~rhs:(Big.close
            (Link.parse_face ["l"; "c1"; "c0"])
            (Big.ppar
               (Big.ppar
                  (Big.nest
                     (Big.ion (Link.parse_face ["c0"]) node1)
                     (Big.par
                        (Big.nest
                           (Big.ion Link.Face.empty ctrl_L)
                           (Big.par
                              (Big.atom n_l ctrl_L_E)
                              id1))
                        id1))
                  (Big.nest
                     (Big.ion (Link.parse_face ["c1"]) node2)
                     (Big.par
                        (Big.nest
                           (Big.ion Link.Face.empty ctrl_L)
                           (Big.par
                              (Big.atom n_l ctrl_L_E)
                              id1))
                        id1)))
               (Big.par
                  (Big.nest
                     (Big.ion (Link.parse_face ["c0"]) ctrl_Conf)
                     (Big.par
                        (Big.atom Link.Face.empty (ctrl_IPv6 addr1))
                        id1))
                  (Big.nest
                     (Big.ion (Link.parse_face ["c1"]) ctrl_Conf)
                     (Big.par
                        (Big.atom Link.Face.empty (ctrl_IPv6 addr2))
                        id1)))))
    None

let new_link = _new_link ctrl_N ctrl_N

let used_new_link = _new_link ctrl_N_U ctrl_N

let used_used_new_link = _new_link ctrl_N_U ctrl_N_U

let _drop_link node1 node2 addr1 addr2 =
  Brs.parse_react_unsafe
    ~lhs:(Big.close
            (Link.parse_face ["l"; "c1"; "c0"])
            (Big.ppar
               (Big.ppar
                  (Big.nest
                     (Big.ion (Link.parse_face ["c0"]) node1)
                     (Big.par
                        (Big.nest
                           (Big.ion Link.Face.empty ctrl_L)
                           (Big.par
                              (Big.atom n_l ctrl_L_E)
                              id1))
                        id1))
                  (Big.nest
                     (Big.ion (Link.parse_face ["c1"]) node2)
                     (Big.par
                        (Big.nest
                           (Big.ion Link.Face.empty ctrl_L)
                           (Big.par
                              (Big.atom n_l ctrl_L_E)
                              id1))
                        id1)))
               (Big.par
                  (Big.nest
                     (Big.ion (Link.parse_face ["c0"]) ctrl_Conf)
                     (Big.par
                        (Big.atom Link.Face.empty (ctrl_IPv6 addr1))
                        id1))
                  (Big.nest
                     (Big.ion (Link.parse_face ["c1"]) ctrl_Conf)
                     (Big.par
                        (Big.atom Link.Face.empty (ctrl_IPv6 addr2))
                        id1)))))
    ~rhs:(Big.close
            (Link.parse_face ["c1"; "c0"])
            (Big.ppar
               (Big.ppar
                  (Big.nest
                     (Big.ion (Link.parse_face ["c0"]) node1)
                     (Big.par
                        (Big.ion Link.Face.empty ctrl_L)
                        id1))
                  (Big.nest
                     (Big.ion (Link.parse_face ["c1"]) node2)
                     (Big.par
                        (Big.ion Link.Face.empty ctrl_L)
                        id1)))
               (Big.par
                  (Big.nest
                     (Big.ion (Link.parse_face ["c0"]) ctrl_Conf)
                     (Big.par
                        (Big.atom Link.Face.empty (ctrl_IPv6 addr1))
                        id1))
                  (Big.nest
                     (Big.ion (Link.parse_face ["c1"]) ctrl_Conf)
                     (Big.par
                        (Big.atom Link.Face.empty (ctrl_IPv6 addr2))
                        id1)))))
    None

let drop_link = _drop_link ctrl_N ctrl_N

let used_drop_link = _drop_link ctrl_N_U ctrl_N

let used_used_drop_link = _drop_link ctrl_N_U ctrl_N_U

let reading_data date t h l p ec eg max_b =
  Big.par_of_list
    [ Big.atom Link.Face.empty (ctrl_Date date);
      Big.atom Link.Face.empty (ctrl_Temperature t);
      Big.atom Link.Face.empty (ctrl_Humidity h);
      Big.atom Link.Face.empty (ctrl_Light_level l);
      Big.atom Link.Face.empty (ctrl_Pressure p);
      Big.atom Link.Face.empty (ctrl_Energy_consumed ec);
      Big.atom Link.Face.empty (ctrl_Energy_generated eg);
      Big.atom Link.Face.empty (ctrl_Max_battery max_b) ]

let reading addr date t h l p ec eg b max_b =
  Brs.parse_react_unsafe
    ~lhs:(Big.nest
            (Big.ion (Link.Face.singleton (Link.Name "c")) ctrl_Conf)
            (Big.par
               (Big.par
                  (Big.atom (Link.Face.empty) (ctrl_IPv6 addr))
                  (Big.ion (Link.Face.empty) ctrl_Sensors))
               id1))
    ~rhs:(Big.nest
            (Big.ion (Link.Face.singleton (Link.Name "c")) ctrl_Conf)
            (Big.par
               (Big.par
                  (Big.atom (Link.Face.empty) (ctrl_IPv6 addr))
                  (Big.nest
                     (Big.ion (Link.Face.empty) ctrl_Sensors)
                     (Big.par
                        (reading_data date t h l p ec eg max_b)
                        (Big.atom (Link.Face.empty) (ctrl_Battery_state b)))))
               id1))
    (Some (Fun.parse [1]))


let reading_depleted addr date t h l p ec eg max_b =
  Brs.parse_react_unsafe
    ~lhs:(Big.nest
            (Big.ion (Link.Face.singleton (Link.Name "c")) ctrl_Conf)
            (Big.par
               (Big.par
                  (Big.atom (Link.Face.empty) (ctrl_IPv6 addr))
                  (Big.ion (Link.Face.empty) ctrl_Sensors))
               id1))
    ~rhs:(Big.nest
            (Big.ion (Link.Face.singleton (Link.Name "c")) ctrl_Conf)
            (Big.par
               (Big.par
                  (Big.atom (Link.Face.empty) (ctrl_IPv6 addr))
                  (Big.nest
                     (Big.ion (Link.Face.empty) ctrl_Sensors)
                     (Big.par
                        (reading_data date t h l p ec eg max_b)
                        (Big.atom (Link.Face.empty) ctrl_Battery_depleted))))
               id1))
    (Some (Fun.parse [1]))

(* Needs reworking to allow moving a node up in the hierarchy
   Rewriting in two steps:
   -  find node and move it outside the topology
   -  find location and move node in *)
let move addr loc =
  Brs.parse_react_unsafe
    ~lhs:(Big.close
            n_c
            (Big.ppar
               (Big.ppar
                  (Big.ion n_c ctrl_N)
                  (Big.ion Link.Face.empty (Ctrl.C (loc, [], 0))))
               (Big.nest
                  (Big.ion n_c ctrl_Conf)
                  (Big.par
                     (Big.atom Link.Face.empty (ctrl_IPv6 addr))
                     id1))))
    ~rhs:(Big.close
            n_c
            (Big.ppar
               (Big.ppar
                  Big.one
                  (Big.nest
                     (Big.ion Link.Face.empty (Ctrl.C (loc, [], 0)))
                     (Big.par
                        (Big.ion n_c ctrl_N)
                        id1)))
               (Big.nest
                  (Big.ion n_c ctrl_Conf)
                  (Big.par
                     (Big.atom Link.Face.empty (ctrl_IPv6 addr))
                     id1))))
    None

let deploy n (x:int) =
  Brs.parse_react_unsafe
    ~lhs:(Big.ion Link.Face.empty ctrl_CONF)
    ~rhs:(Big.nest
            (Big.ion Link.Face.empty ctrl_CONF)
            (Big.par
               (Big.nest
                  (Big.ion Link.Face.empty (ctrl_App n))
                  (Big.atom Link.Face.empty (ctrl_A x)))
               id1))
    None

let _deploy_on_node node (x:int) addr =
  Brs.parse_react_unsafe
    ~lhs:(Big.close
            n_c
            (Big.ppar
               (Big.ion n_c node)
               (Big.nest
                  (Big.ion n_c ctrl_Conf)
                  (Big.par
                     (Big.atom Link.Face.empty (ctrl_IPv6 addr))
                     id1))))
    ~rhs:(Big.close
            n_c
            (Big.ppar
               (Big.nest
                  (Big.ion n_c ctrl_N_U)
                  (Big.par
                     (Big.atom Link.Face.empty (ctrl_A x))
                     id1))
               (Big.nest
                  (Big.ion n_c ctrl_Conf)
                  (Big.par
                     (Big.atom Link.Face.empty (ctrl_IPv6 addr))
                     id1))))
    None

let deploy_on_node = _deploy_on_node ctrl_N
    
let deploy_on_used_node = _deploy_on_node ctrl_N_U

let double_links addr1 addr2 =
  Brs.parse_react_unsafe
    ~lhs:(Big.close
            (Link.parse_face ["l1"; "l0"; "c1"; "c0"])
            (Big.ppar
               (Big.par
                  (Big.nest
                     (Big.ion (Link.parse_face ["c0"]) ctrl_N)
                     (Big.par
                        (Big.nest
                           (Big.ion Link.Face.empty ctrl_L)
                           (Big.par
                              (Big.par
                                 (Big.atom (Link.parse_face ["l0"]) ctrl_L_E)
                                 (Big.atom (Link.parse_face ["l1"]) ctrl_L_E))
                              id1))
                        id1))
                  (Big.nest
                     (Big.ion (Link.parse_face ["c1"]) ctrl_N)
                     (Big.par
                        (Big.nest
                           (Big.ion Link.Face.empty ctrl_L)
                           (Big.par
                              (Big.par
                                 (Big.atom (Link.parse_face ["l0"]) ctrl_L_E)
                                 (Big.atom (Link.parse_face ["l1"]) ctrl_L_E))
                              id1))
                        id1)))
               (Big.par
                  (Big.nest
                     (Big.ion (Link.parse_face ["c0"]) ctrl_Conf)
                     (Big.par
                        (Big.atom Link.Face.empty (ctrl_IPv6 addr1))
                        id1))
                  (Big.nest
                     (Big.ion (Link.parse_face ["c1"]) ctrl_Conf)
                     (Big.par
                        (Big.atom Link.Face.empty (ctrl_IPv6 addr2))
                        id1)))))
    ~rhs:(Big.close
            (Link.parse_face ["l0"; "c1"; "c0"])
            (Big.ppar
               (Big.par
                  (Big.nest
                     (Big.ion (Link.parse_face ["c0"]) ctrl_N)
                     (Big.par
                        (Big.nest
                           (Big.ion Link.Face.empty ctrl_L)
                           (Big.par
                              (Big.atom (Link.parse_face ["l0"]) ctrl_L_E)
                              id1))
                        id1))
                  (Big.nest
                     (Big.ion (Link.parse_face ["c1"]) ctrl_N)
                     (Big.par
                        (Big.nest
                           (Big.ion Link.Face.empty ctrl_L)
                           (Big.par
                              (Big.atom (Link.parse_face ["l0"]) ctrl_L_E)
                              id1))
                        id1)))
               (Big.par
                  (Big.nest
                     (Big.ion (Link.parse_face ["c0"]) ctrl_Conf)
                     (Big.par
                        (Big.atom Link.Face.empty (ctrl_IPv6 addr1))
                        id1))
                  (Big.nest
                     (Big.ion (Link.parse_face ["c1"]) ctrl_Conf)
                     (Big.par
                        (Big.atom Link.Face.empty (ctrl_IPv6 addr2))
                        id1)))))
    None

let double_token x =
  Brs.parse_react_unsafe
    ~lhs:(Big.nest
            (Big.ion n_c ctrl_N_U)
            (Big.par
               (Big.par
                  (Big.atom Link.Face.empty (ctrl_A x))
                  (Big.atom Link.Face.empty (ctrl_A x)))
               id1))
    ~rhs:(Big.nest
            (Big.ion n_c ctrl_N_U)
            (Big.par
               (Big.atom Link.Face.empty (ctrl_A x))
               id1))
    None

let broken_link =
  Brs.parse_react_unsafe
    ~lhs:(Big.close n_l (Big.atom n_l ctrl_L_E))
    ~rhs:Big.one
    None

(* Predicates *)

(* App id requires at least n nodes at location loc *)
let app_req_1 id loc n =
  let token =
    Big.nest
      (Big.ion n_c ctrl_N_U)
      (Big.par
         (Big.ion Link.Face.empty (ctrl_A id))
         id1) in
  Big.nest
    (Big.ion Link.Face.empty (Ctrl.C (loc, [], 0)))
    (Big.par_of_list (id1 :: (Misc.init_list token n [])))

let battery_below x =
  Big.ion (Link.parse_face []) (ctrl_Battery_state x)

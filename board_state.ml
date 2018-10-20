
open Board
open Player

module String_map = Map.Make (String)
module String_set = Map.Make (String)
module Player_map = Map.Make (Player)

type army = int

(* idea: we want to be able to access this information quickly
   from both ends; so we implement data structures that facilitate
   both sides of data access and update them at the same time *)

type node_state = {owner : Player.t option; army : army}
type cont_state = {owner : Player.t option}
type player_state = {
  nodes : node_id String_set.t;
  conts : cont_id String_set.t
}

type t = {
  board : Board.t;
  nodes : node_state String_map.t;
  conts : cont_state String_map.t;
  players : player_state Player_map.t;
}

exception UnknownPlayer of Player.t

let init board = failwith "todo"

let board ({board} : t) = board

let node_state ({nodes} : t) node_id =
  match String_map.find_opt node_id nodes with
  | Some (state) -> state
  | None -> raise (UnknownNode node_id)

let cont_state ({conts} : t) cont_id =
  match String_map.find_opt cont_id conts with
  | Some (state) -> state
  | None -> raise (UnknownCont cont_id)

let player_state ({players} : t) player =
  match Player_map.find_opt player players with
  | Some (state) -> state
  | None -> raise (UnknownPlayer player)

let node_owner st node =
  let ({owner} : node_state) = (node_state st node) in owner

let node_army st node =
  let ({army} : node_state) = (node_state st node) in army

let cont_owner st cont =
  let ({owner} : cont_state) = (cont_state st cont) in owner

let player_nodes st player =
  failwith "todo"

let player_conts st player =
  failwith "todo"

let player_army st player =
  failwith "todo"

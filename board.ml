
open Yojson.Basic.Util

module String_map = Map.Make (String)

type node_id = string

type cont_id = string

(* note: storing node_id in node and cont seems redundant now,
   but it could come in handy later *)

type node = {id : node_id; name : string; borders : node_id list}

type cont = {id : cont_id; name : string; nodes : node_id list}

type t = {name : string; nodes : node String_map.t; conts : cont String_map.t}

exception UnknownNode of node_id

exception UnknownCont of cont_id

let from_json json =
  failwith "todo"

let board_name ({name} : t) = name

(* todo this is probably inefficient *)
let list_of_string_map map = List.map (fun (k, _) -> k) (String_map.bindings map)

let nodes ({nodes} : t) = list_of_string_map nodes

let find_node ({nodes} : t) (node : node_id) =
  match String_map.find_opt node nodes with
  | Some (node) -> node
  | None -> raise (UnknownNode node)

let node_borders board node = let {borders} : node = (find_node board node) in borders

let node_name board node = let {name} : node = (find_node board node) in name

let conts ({conts} : t) = list_of_string_map conts

let find_cont ({conts} : t) (cont : cont_id) =
  match String_map.find_opt cont conts with
  | Some (cont) -> cont
  | None -> raise (UnknownCont cont)

let cont_nodes board cont = let {nodes} : cont = (find_cont board cont) in nodes

let cont_name board cont = let {name} : cont = (find_cont board cont) in name

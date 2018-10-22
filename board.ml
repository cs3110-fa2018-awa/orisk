
open Yojson.Basic.Util

module String_map = Map.Make (String)

type coords = (int * int)
(* for accessing coords *)
let x = fst
let y = snd
let make_coords x y = x, y

type node_id = string
type cont_id = string

type army = int

(* note: storing node_id in node and cont seems redundant now,
   but it could come in handy later *)

type node = {id : node_id; name : string; coords : coords;  borders : node_id list}
type cont = {id : cont_id; name : string; bonus : army; nodes : node_id list}
type t = {name : string; ascii : string; nodes : node String_map.t; conts : cont String_map.t}

exception UnknownNode of node_id
exception UnknownCont of cont_id

let coords_of_node json =
  (json |> member "x" |> to_int), (json |> member "y" |> to_int)

let node_of_json json =
  {
    id = json |> member "id" |> to_string;
    name = json |> member "name" |> to_string;
    coords = json |> member "coordinates" |> coords_of_node;
    borders = json |> member "borders" |> to_list |> List.map to_string;
  }

let cont_of_json json =
  {
    id = json |> member "id" |> to_string;
    name = json |> member "name" |> to_string;
    bonus = json |> member "bonus" |> to_int;
    nodes = json |> member "territories" |> to_list |> List.map to_string;
  }

let rec node_to_map m = function
  | [] -> m
  | ({id} : node) as node :: tl
    -> node_to_map (String_map.add id node m) tl

let rec cont_to_map m = function
  | [] -> m
  | ({id} : cont) as cont :: tl
    -> cont_to_map (String_map.add id cont m) tl

let from_json json =
  {
    name = json |> member "map" |> to_string;
    ascii = json |> member "ascii" |> to_string;
    nodes = json |> member "territories" |> to_list
            |> List.map node_of_json |> node_to_map String_map.empty;
    conts = json |> member "continents" |> to_list
            |> List.map cont_of_json |> cont_to_map String_map.empty;
  }

let board_name ({name} : t) = name

let board_ascii ({ascii} : t) = ascii

(* todo this is probably inefficient *)
let list_of_string_map map = List.map (fun (k, _) -> k) (String_map.bindings map)

let fold_internal (f : 'a -> 'b -> 'b) (acc : 'b) (map : 'c String_map.t) =
  String_map.fold (fun (a:'a) (_:'c) (b:'b) -> f a b) map acc

let nodes ({nodes} : t) = list_of_string_map nodes 

let fold_nodes ({nodes} : t) (f : node_id -> 'a -> 'a) (acc : 'a) : 'a =
  fold_internal f acc nodes

let has_node ({nodes} : t) node = String_map.mem node nodes

let find_node ({nodes} : t) (node_id : node_id) =
  match String_map.find_opt node_id nodes with
  | Some (node) -> node
  | None -> raise (UnknownNode node_id)

let node_borders board node_id = (find_node board node_id).borders

let node_name board node_id = (find_node board node_id).name

let node_coords board node_id = (find_node board node_id).coords

let conts ({conts} : t) = list_of_string_map conts

let fold_conts ({conts} : t) (f : cont_id -> 'a -> 'a) (acc : 'a) : 'a =
  fold_internal f acc conts

let has_cont ({conts} : t) cont = String_map.mem cont conts

let find_cont ({conts} : t) (cont_id : cont_id) =
  match String_map.find_opt cont_id conts with
  | Some (cont) -> cont
  | None -> raise (UnknownCont cont_id)

let cont_nodes board cont = (find_cont board cont).nodes

let cont_name board cont = (find_cont board cont).name

let cont_bonus board cont = (find_cont board cont).bonus

let node_conts board node =
  (* TODO perhaps this could be computed ahead of time *)
  fold_conts board
    (fun cont_id acc -> if List.mem node
        (let ({nodes} : cont) = (find_cont board cont_id) in nodes)
      then cont_id :: acc else acc) []

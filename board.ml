
open Yojson.Basic.Util

(** ['a String_map] is used for storing maps indexed by [node_id]
    or [node_id], both of which are aliases of [string]. *)
module String_map = Map.Make (String)

(** The type of a coordinate pair, an int tuple of (x, y). *)
type coords = (int * int)

(** [x] is the first item in the (x, y) coords tuple. *)
let x = fst

(** [y] is the second item in the (x, y) coords tuple. *)
let y = snd

(** [make_coords x y] is the coords tuple (x, y). This is a convenience
    function if we want to change the implementation of [coords] in the
    future. *)
let make_coords x y = x, y

(** The type of ID for referring to nodes (aka territories). *)
type node_id = string

(** The type of ID for referring to continents. *)
type cont_id = string

(** The type of army size, an alias for int. *)
type army = int

(* note: storing node_id in node and cont_id in cont seems redundant now,
   but it could come in handy later *)

(** The type of node, used internally. An ID, a name,
    a coords pair, and a list of bordering nodes. *)
type node = {
  id : node_id;
  name : string;
  coords : coords;
  borders : node_id list
}

(** The type of cont, used internally. An ID, a name,
    a bonus reinforcement size, and a list of contained nodes. *)
type cont = {
  id : cont_id;
  name : string;
  bonus : army;
  nodes : node_id list
}

(** The type of board. A name, an ASCII art string, the height
    of the ASCII art string (calculated in advance), a map
    of nodes, and a map of continents. *)
type t = {
  name : string;
  ascii : string;
  ascii_height : int;
  nodes : node String_map.t;
  conts : cont String_map.t
}

(** [UnknownNode node] is the exception raised when [node] is not found. *)
exception UnknownNode of node_id

(** [UnknownCont cont] is the exception raised when [cont] is not found. *)
exception UnknownCont of cont_id

(** [coords_of_node json] is the coords value represented by [json]. *)
let coords_of_node json =
  (json |> member "x" |> to_int), (json |> member "y" |> to_int)

(** [node_of_json json] is the node value represented by [json]. *)
let node_of_json json =
  {
    id = json |> member "id" |> to_string;
    name = json |> member "name" |> to_string;
    coords = json |> member "coordinates" |> coords_of_node;
    borders = json |> member "borders" |> to_list |> List.map to_string;
  }

(** [cont_of_json json] is the continent value represented by [json]. *)
let cont_of_json json =
  {
    id = json |> member "id" |> to_string;
    name = json |> member "name" |> to_string;
    bonus = json |> member "bonus" |> to_int;
    nodes = json |> member "territories" |> to_list |> List.map to_string;
  }

(** [node_list_to_map m lst] is the map [m] with the nodes in
    [lst] added. *)
let rec node_list_to_map m = function
  | [] -> m
  | ({id} : node) as node :: tl
    -> node_list_to_map (String_map.add id node m) tl

(** [cont_list_to_map m lst] is the map [m] with the continents
    in [lst] added. *)
let rec cont_list_to_map m = function
  | [] -> m
  | ({id} : cont) as cont :: tl
    -> cont_list_to_map (String_map.add id cont m) tl

(** [count_newlines str] is the number of newline characters in [str]. *)
let count_newlines str = str |> Str.split (Str.regexp "\n") |> List.length

(** [from_json json] is the board represented by [json]. Each of the fields
    is populated as expected using the helper functions above, except for
    [ascii_height], which is calculated from the [ascii] field. *)
let from_json json =
  let ascii =  json |> member "ascii" |> to_string in
  {
    name = json |> member "map" |> to_string;
    ascii = ascii;
    ascii_height = (count_newlines ascii) + 1;
    nodes = json |> member "territories" |> to_list
            |> List.map node_of_json |> node_list_to_map String_map.empty;
    conts = json |> member "continents" |> to_list
            |> List.map cont_of_json |> cont_list_to_map String_map.empty;
  }

(** [board_node board] is the name of [board]. *)
let board_name board = board.name

(** [board_ascii board] is the ASCII art string associated with [board]. *)
let board_ascii board = board.ascii

(** [board_ascii_height board] is the number of lines in the ASCII art
    associated with [board]. *)
let board_ascii_height board = board.ascii_height

(** [list_of_string_map map] is the string list containing all of the elements
    of [map].

    TODO the current implementation is inefficient because it generates a
    new list every time that it is applied. *)
let list_of_string_map map = List.map (fun (k, _) -> k) (String_map.bindings map)

(** [fold_internal f acc map] tail-recursively folds over [map] with function
    [f] and accumulator [acc]. *)
let fold_internal (f : 'a -> 'b -> 'b) (acc : 'b) (map : 'c String_map.t) =
  String_map.fold (fun (a:'a) (_:'c) (b:'b) -> f a b) map acc

(** [nodes board] is the list of all nodes in [board]. *)
let nodes board = list_of_string_map board.nodes 

(** [fold_nodes board f acc] is a tail-recursive fold over all of the nodes
    in [board] with accumulator [acc]. [f] is a function that takes a node
    ID and an accumulator and produces the next accumulator. *)
let fold_nodes board (f : node_id -> 'a -> 'a) (acc : 'a) : 'a =
  fold_internal f acc board.nodes

(** [has_node board node] is true iff [node] is a node of [board]. *)
let has_node board node = String_map.mem node board.nodes

(** [find_node board node] is the node object referenced by the ID
    [node] in [board].

    Raises [UnknownNode node] iff [has_node board node] is false. *)
let find_node board (node_id : node_id) =
  match String_map.find_opt node_id board.nodes with
  | Some (node) -> node
  | None -> raise (UnknownNode node_id)

(** [node_borders board node] is the list of nodes bordering [node]
    in [board].

    Raises [UnknownNode node] iff [has_node board node] is false. *)
let node_borders board node_id = (find_node board node_id).borders

(** [node_name board node] is the name of [node] in [board].

    Raises [UnknownNode node] iff [has_node board node] is false. *)
let node_name board node_id = (find_node board node_id).name

(** [node_coords board node] is the coordinates of [node] in [board].

    Raises [UnknownNode node] iff [has_node board node] is false. *)
let node_coords board node_id = (find_node board node_id).coords

(** [conts board] is the list of all continents in [board]. *)
let conts board = list_of_string_map board.conts

(** [fold_conts board f acc] is a tail-recursive fold over all of the
    continents in [board] with accumulator [acc]. [f] is a function that
    takes a node ID and an accumulator and produces the next accumulator. *)
let fold_conts board (f : cont_id -> 'a -> 'a) (acc : 'a) : 'a =
  fold_internal f acc board.conts

(** [has_cont board cont] is true iff [cont] is a continent of [board]. *)
let has_cont board cont = String_map.mem cont board.conts

(** [find_cont board cont] is the continent object referenced by the ID
    [cont] in [board]. *)
let find_cont board (cont_id : cont_id) =
  match String_map.find_opt cont_id board.conts with
  | Some (cont) -> cont
  | None -> raise (UnknownCont cont_id)

(** [cont_nodes board cont] is the list of all nodes in [cont] of [board].

    Raises [UnknownCont cont] iff [has_cont board cont] is false. *)
let cont_nodes board cont = (find_cont board cont).nodes

(** [cont_name board cont] is the name of [cont] in [board].

    Raises [UnknownCont cont] iff [has_cont board cont] is false. *)
let cont_name board cont = (find_cont board cont).name

(** [cont_bonus board cont] is the number of bonus armies provided
    to the player controlling the entirety of [cont] in [board].

    Raises [UnknownCont cont] iff [has_cont board cont] is false. *)
let cont_bonus board cont = (find_cont board cont).bonus

(** [node_conts board node] is the list of continents containing [node]
    in [board].

    Raises [UnknownNode node] iff [has_node board node] is false.

    TODO perhaps this could be computed ahead of time to speed things up. *)
let node_conts board node =
  fold_conts board
    (fun cont_id acc -> if List.mem node
        (let ({nodes} : cont) = (find_cont board cont_id) in nodes)
      then cont_id :: acc else acc) []
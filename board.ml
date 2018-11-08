open Yojson.Basic.Util

(** ['a String_map] is used for storing maps indexed by [node_id]
    or [cont_id], both of which are aliases of [string]. *)
module String_map = Map.Make (String)

(** The type of a coordinate pair (x, y). *)
type coords = (int * int)

(** [x (a,b)] is [a]. This is used for accessing values from [coords]. *)
let x = fst

(** [y (a,b)] is [b]. This is used for accessing values from [coords]. *)
let y = snd

(** [make_coords x y] is the coords tuple (x, y). This is a convenience
    function if we want to change the implementation of [coords] in the
    future. *)
let make_coords x y = x,y

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

(** The type of board. A name, an ASCII art string, the height and width
    of the ASCII art string (calculated in advance), a map
    of nodes, and a map of continents. *)
type t = {
  name : string;
  ascii : string;
  ascii_height : int;
  ascii_width : int;
  nodes : node String_map.t;
  conts : cont String_map.t
}

(** [UnknownNode node] is the exception raised when [node] is not found. *)
exception UnknownNode of node_id

(** [UnknownCont cont] is the exception raised when [cont] is not found. *)
exception UnknownCont of cont_id

(** [coords_of_node json] is the [coords] value represented by [json]. *)
let coords_of_node json =
  try (json |> member "x" |> to_int), (json |> member "y" |> to_int) with
  | Yojson.Basic.Util.Type_error (msg, j) ->
    j |> Yojson.Basic.to_string |> print_endline;
    failwith ("failed to load board coords: " ^ msg)

let json_of_coords (x, y) =
  `Assoc [("x", `Int x); ("y", `Int y)]

(** [node_of_json json] is the [node] value represented by [json]. *)
let node_of_json json =
  try begin
    {
      id = json |> member "id" |> to_string;
      name = json |> member "name" |> to_string;
      coords = json |> member "coordinates" |> coords_of_node;
      borders = json |> member "borders" |> to_list |> List.map to_string;
    }
  end with
  | Yojson.Basic.Util.Type_error (msg, j) ->
    j |> Yojson.Basic.to_string |> print_endline;
    failwith ("failed to load board node: " ^ msg)

let json_of_node (node : node) =
  `Assoc [
    ("id", `String node.id);
    ("name", `String node.name);
    ("coordinates", json_of_coords node.coords);
    ("borders", `List (List.map (fun border -> `String border) node.borders))
  ]

(** [cont_of_json json] is the [cont] value represented by [json]. *)
let cont_of_json json =
  try begin
    {
      id = json |> member "id" |> to_string;
      name = json |> member "name" |> to_string;
      bonus = json |> member "bonus" |> to_int;
      nodes = json |> member "territories" |> to_list |> List.map to_string;
    }
  end with
  | Yojson.Basic.Util.Type_error (msg, j) ->
    j |> Yojson.Basic.to_string |> print_endline;
    failwith ("failed to load board cont: " ^ msg)

let json_of_cont (cont : cont) =
  `Assoc [
    ("id", `String cont.id);
    ("name", `String cont.name);
    ("bonus", `Int cont.bonus);
    ("territories", `List (List.map (fun border -> `String border) cont.nodes))
  ]

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

(** [newline_regexp] is the regular expression for new lines.*)
let newline_regexp = Str.regexp "\n"

(** [board_ascii_lines board] is the list of lines in [board_ascii board]
    (i.e. the ascii split by the newline character). *)
let board_ascii_lines board = board.ascii |> Str.split newline_regexp

(** [count_newlines str] is the number of newline characters in [str]. *)
let count_newlines str = str |> Str.split newline_regexp |> List.length

(** [count_width str] is the number of characters across one line in [str]. 
    Requires that the ascii is "rectangular", i.e. each line in [str] contains
    the same number of characters. *)
let count_width str = 
  str |> Str.split newline_regexp |> List.hd |> String.length

(** [from_json json] is the board represented by [json]. Each of the fields
    is populated as expected using the helper functions above, except for
    [ascii_height], which is calculated from the [ascii] field. *)
let from_json json =
  try begin
    let ascii = json |> member "ascii" |> to_string in
    {
      name = json |> member "map" |> to_string;
      ascii = ascii;
      ascii_height = (count_newlines ascii) + 1;
      ascii_width = count_width ascii;
      nodes = json |> member "territories" |> to_list
              |> List.map node_of_json |> node_list_to_map String_map.empty;
      conts = json |> member "continents" |> to_list
              |> List.map cont_of_json |> cont_list_to_map String_map.empty;
    }
  end with
  | Yojson.Basic.Util.Type_error (msg, j) ->
    j |> Yojson.Basic.to_string |> print_endline;
    failwith ("failed to load board: " ^ msg)

let json_of_board board : Yojson.Basic.json =
  `Assoc [
    ("map", `String board.name);
    ("ascii", `String board.ascii);
    ("territories", `List (board.nodes |> String_map.bindings
                     |> List.map (fun (_, node) -> json_of_node node)));
    ("continents", `List (board.conts |> String_map.bindings
                     |> List.map (fun (_, cont) -> json_of_cont cont)));
  ]

(** [board_node board] is the name of [board]. *)
let board_name board = board.name

(** [board_ascii board] is the ASCII art string associated with [board]. *)
let board_ascii board = board.ascii

(** [board_ascii_height board] is the number of lines in the ASCII art
    associated with [board]. *)
let board_ascii_height board = board.ascii_height

(** [board_ascii_width board] is the number of characters in one line
    of the ASCII art associated with [board]. *)
let board_ascii_width board = board.ascii_width

(** [list_of_string_map map] is the string list containing all of the elements
    of [map]. *)
let list_of_string_map map = 
  List.map (fun (k, _) -> k) (String_map.bindings map)

(** [fold_internal f acc map] tail-recursively folds over [map] with function
    [f] and accumulator [acc]. *)
let fold_internal (f : 'a -> 'b -> 'b) (acc : 'b) (map : 'c String_map.t) =
  String_map.fold (fun (a:'a) (_:'c) (b:'b) -> f a b) map acc

(** [nodes board] is the list of all nodes in [board]. *)
let nodes board = list_of_string_map board.nodes 

(** [fold_nodes board f acc] is a tail-recursive fold over all of the nodes
    in [board] with accumulator [acc]. *)
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
    continents in [board] with accumulator [acc]. *)
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

    Raises [UnknownNode node] iff [has_node board node] is false.*)
let node_conts board node =
  fold_conts board
    (fun cont_id acc -> if List.mem node
        (let ({nodes} : cont) = (find_cont board cont_id) in nodes)
      then cont_id :: acc else acc) []

(** [start_with start compare_to] performs a structural comparison
    of the strings [start] and [compare_to]. *)
let start_with start compare_to = 
  if String.length start > String.length compare_to then false 
  else Str.string_before compare_to (String.length start) = start

(** [node_search board str] is [Some node_id] of the alphabetically
    first node that begins with [str], or [None] if such a node could
    not be found. *)
let node_search board str =
  (* get only nodes starting with str *)
  String_map.filter (fun k _ -> start_with str k) board.nodes
  (* get first alphabetically *)
  |> String_map.min_binding_opt |> function 
  | Some (k,_) -> Some k
  | None -> None

(** [nodes_filter board predicate] is the list of nodes in [board]
    for which [predicate node_id] is true. *)
let nodes_filter board predicate =
  String_map.filter (fun k v -> predicate k) board.nodes |> list_of_string_map

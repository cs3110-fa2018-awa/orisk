
(** The type of a board. *)
type t

type coords = (int * int)

(** The type of identifier for a node. *)
type node_id = string

(** The type of a continent/ *)
type cont

(** The type of identifer for a continent. *)
type cont_id = string

type army = int

val from_json : Yojson.Basic.json -> t

val x : coords -> int

val y : coords -> int

val board_name : t -> string

val board_ascii : t -> string

val nodes : t -> node_id list

val fold_nodes : t -> (node_id -> 'a -> 'a) -> 'a -> 'a

val has_node : t -> node_id -> bool

val node_borders : t -> node_id -> node_id list

val node_name : t -> node_id -> string

val node_coords : t -> node_id -> coords

val conts : t -> cont_id list

val fold_conts : t -> (cont_id -> 'a -> 'a) -> 'a -> 'a

val has_cont : t -> cont_id -> bool

val cont_nodes : t -> cont_id -> node_id list

val cont_name : t -> cont_id -> string

val cont_bonus : t -> cont_id -> army

val node_conts : t -> node_id -> cont_id list

exception UnknownNode of node_id

exception UnknownCont of cont_id


(** The type of a board. *)
type t

(** The type of identifier for a node. *)
type node_id

(** The type of a continent/ *)
type cont

(** The type of identifer for a continent. *)
type cont_id

val from_json : Yojson.Basic.json -> t

val board_name : t -> string

val nodes : t -> node_id list

val node_borders : t -> node_id -> node_id list

val node_name : t -> node_id -> string

val conts : t -> cont_id list

val cont_nodes : t -> cont_id -> node_id list

val cont_name : t -> cont_id -> string

exception UnknownNode of node_id

exception UnknownCont of cont_id

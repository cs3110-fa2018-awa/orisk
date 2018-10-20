
module type Board = sig
  (** The type of a board. *)
  type t

  (** The type of identifier for a node. *)
  type node_id

  val board_name : t -> string

  val nodes : t -> node_id list

  val borders : t -> node_id -> node_id list

  val node_name : t -> node_id -> string

  exception UnknownNode of node_id
end

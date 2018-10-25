(** Representation of a player.*)

(** The abtract type representing a player. *)
type t

(** Type alias for terminal colors. *)
type color = ANSITerminal.color

(** [create name color] is the player with [name] and [color]. *)
val create : string -> color -> t

(** [player_name player] is the [name] of [player]. *)
val player_name : t -> string

(** [player_color player] is the [color] of [player]. *)
val player_color : t -> color

(** [compare p1 p2] is the result of [Pervasives.compare] with the
    order in which the players were created. This allows players to
    be stored in binary search trees. *)
val compare : t -> t -> int

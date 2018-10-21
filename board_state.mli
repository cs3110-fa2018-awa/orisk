
open Board
open Player

type t

type army = int

(** [init b players] is the default state from board [b]. *)
val init : Board.t -> Player.t list -> t

(** [board s] is the board used by state [s]. *)
val board : t -> Board.t

val node_owner : t -> node_id -> Player.t option

val node_army : t -> node_id -> army

val cont_owner : t -> cont_id -> Player.t option

val player_nodes : t -> Player.t -> node_id list

val player_conts : t -> Player.t -> cont_id list

(** [player_army state player] is the total number of armies controlled
    by [player] in [state]. *)
val player_army : t -> Player.t -> army

(** [set_army state node army] is the new state resulting from setting
    [node] to have [army] armies in [state]. *)
val set_army : t -> node_id -> army -> t

(** [place_army state node army] is the new state resulting from adding
    [army] armies to [node] in [state]. *)
val place_army : t -> node_id -> army -> t

(** [set_owner state node player] is the new state resulting from
    changing ownership of [node] to [player] in [state]. *)
val set_owner : t -> node_id -> Player.t option -> t

exception UnknownPlayer of Player.t

(** Representation of a game interface. *)

open Board
open Board_state

type t

type arrow = Up | Down | Left | Right

val init : Game_state.t -> t

val game_state : t -> Game_state.t

val board_state : t -> Board_state.t

val board : t -> Board.t

(** [leaderboard_on st] is whether or not the leaderboard is activated in 
    [st]. *)
val leaderboard_on : t -> bool

(** [leaderboard_cat st] is the category that the leaderboard is sorted by in 
    [st]. *)
val leaderboard_cat : t -> stats_category

(** [toggle_leaderboard st] is the interface with the leaderboard activation
    opposite of the one in [st]. *)
val toggle_leaderboard : t -> t

(** [set_leaderboard_cat st cat] is the interface [st] with the sorted by 
    category set to [cat]. *)
val set_leaderboard_cat : t -> stats_category -> t

(** [help_on st] is whether the help menu is activated in [st]. *)
val help_on : t -> bool

(** [help_cat st] is the category of gameplay state in [st] that help 
    is being displayed for. *)
val help_cat : t -> string

(** [toggle_help st] is the interface with the help activation opposite
    of the one in [st]. *)
val toggle_help : t -> t

val attacking_node : t -> node_id option

val change_attack_node : t -> node_id option -> t

val from_fortify_node : t -> node_id option

val change_from_fortify_node : t -> node_id option -> t

val cursor : t -> coords

val cursor_node : t -> node_id

val scroll : t -> coords

val scroll_by : t -> int -> int -> t

val gs : t -> Game_state.t -> t

val move_arrow : t -> arrow -> t

val pick : t -> t

val change_game_st : t -> Game_state.t -> t

val set_cursor_node : t -> node_id option -> t

val turn_valid_nodes : t -> node_id list

val reinforce_place : t -> node_id option -> t

val fortify_select : t -> node_id option -> node_id option -> t

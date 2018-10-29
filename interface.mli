open Board
open Board_state

type t

type arrow = Up | Down | Left | Right

val init : Game_state.t -> t

val game_state : t -> Game_state.t

val board_state : t -> Board_state.t

val board : t -> Board.t

val leaderboard_on : t -> bool

val leaderboard_cat : t -> stats_category

val toggle_leaderboard : t -> t

val set_leaderboard_cat : t -> stats_category -> t

val attacking_node : t -> node_id option

val change_attack_node : t -> node_id option -> t

val from_fortify_node : t -> node_id option

val change_from_fortify_node : t -> node_id option -> t

val cursor : t -> coords

val cursor_node : t -> node_id

val scroll : t -> coords

val gs : t -> Game_state.t -> t

val move_arrow : t -> arrow -> t

val pick : t -> t

val change_game_st : t -> Game_state.t -> t

val set_cursor_node : t -> node_id option -> t

val turn_valid_nodes : t -> node_id list

val reinforce_place : t -> node_id option -> t

val fortify_select : t -> node_id option -> node_id option -> t


open Board

type t

type arrow = Up | Down | Left | Right

val init : Game_state.t -> t

val game_state : t -> Game_state.t

val cursor : t -> coords

val cursor_node : t -> node_id

val scroll : t -> coords

val gs : t -> Game_state.t -> t

val move_arrow : t -> arrow -> t

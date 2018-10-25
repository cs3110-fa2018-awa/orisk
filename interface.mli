
open Board

type reinforce_step = SelectR | PlaceR
type attack_step = AttackSelectA | DefendSelectA | ResultA | OccupyA
type fortify_step = FromSelectF | ToSelectF | CountF

type t

val init : Game_state.t -> t

val game_state : t -> Game_state.t

val cursor : t -> coords

val cursor_node : t -> node_id

val scroll : t -> coords

val reinforcestep : t -> reinforce_step

val attack_step : t -> attack_step

val fortify_step : t -> fortify_step

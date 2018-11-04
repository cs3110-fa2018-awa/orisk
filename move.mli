
open Board

type move =
  | PickM of node_id
  | ReinforceM of node_id * army
  | AttackM of node_id * node_id * army
  | OccupyM of army
  | Fortify of node_id * node_id * army
  | Finish

val apply_move : Game_state.t -> move -> Game_state.t

val valid_fortifications : Game_state.t -> move list

val valid_moves : Game_state.t -> move list

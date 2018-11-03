
open Board

type move =
  | PickM of node_id
  | ReinforceM of node_id
  | AttackM of node_id * node_id
  | OccupyM of army
  | Fortify of node_id * node_id

val apply_move : Game_state.t -> move -> Game_state.t

val valid_moves : Game_state.t -> move list

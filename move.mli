(** Moves for AI. *)

open Board

type move =
  | PickM of node_id
  | TradeM of int
  | ReinforceM of (node_id * army) list
  | AttackM of node_id * node_id * army
  | OccupyM of army
  | FortifyM of node_id * node_id * army
  | FinishM

val apply_move : Game_state.t -> move -> Game_state.t

val valid_fortifications : Game_state.t -> move list

val valid_moves : Game_state.t -> move list

val string_of_move : move -> string

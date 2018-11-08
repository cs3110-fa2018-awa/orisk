(** Moves for AI. *)

open Board

(** [move] is the type of move a player can perform in the game. *)
type move =
  | PickM of node_id
  | TradeM of int
  | ReinforceM of (node_id * army) list
  | AttackM of node_id * node_id * army
  | OccupyM of army
  | FortifyM of node_id * node_id * army
  | FinishM

(** [string_of_move move] is the [string] representation of [move]. *)
val string_of_move : move -> string

(** [apply_move gs move] is the game state resulting from applying [move] in
    [gs]. *)
val apply_move : Game_state.t -> move -> Game_state.t

(** [valid_moves gs] is a list of valid [moves] that can be applied by
    the current player of [gs]. *)
val valid_moves : Game_state.t -> move list
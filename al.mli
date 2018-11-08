(** AI (named Al) for the game. *)

(** [best_move gs depth] is the [move] with the highest heuristic score
    that the current player of [gs] can perform. Looks [depth] moves ahead when 
    computing heuristic. *)
val best_move : Game_state.t -> int -> Move.move
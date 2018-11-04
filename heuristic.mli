
open Move

type score = float

val heuristic : Game_state.t -> Personality.t -> score

val best_move : Game_state.t -> Personality.t -> move

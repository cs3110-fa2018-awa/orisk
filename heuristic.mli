
open Board
open Move

type score = float

val heuristic : Game_state.t -> Personality.t -> Player.t -> score

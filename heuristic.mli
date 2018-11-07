(** Heuristic for Al. *)

open Board
open Move

(** Type alias for heuristic score *)
type score = float

(** [heuristic gs personality player] is the [score] of [player] with 
    [personality] in [gs]. *)
val heuristic : Game_state.t -> Personality.t -> Player.t -> score

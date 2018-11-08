(** Representation of the personality of the AI. *)

(** The abstract type representing the personality of a player. *)
type t

(** [default] is the default [t]. *)
val default : t

(** [node_heuristic p num] is the heuristic score of [num] nodes using [p]. *)
val node_heuristic : t -> int -> float

(** [bonus_heuristic p num] is the heuristic score of [num] bonus armies
    using [p]. *)
val bonus_heuristic : t -> int -> float

(** [army_heuristic p num] is the heuristic score of [num] armies using [p]. *)
val army_heuristic : t -> int -> float

(** [region_heuristic p num] is the heuristic score of [num] distinct contiguous 
    regions using [p]. *)
val region_heuristic : t -> int -> float

(** [frontier_heuristic p num] is the heuristic score of [num] frontier nodes
    using [p]. *)
val frontier_heuristic : t -> int -> float

(** [frontier_armies_heuristic p num] is the heuristic score of [num]
    armies on frontier nodes using [p]. *)
val frontier_armies_heuristic : t -> int -> float

(** [avg_frontier_armies_heuristic p num] is the heuristic score of [num] 
    average frontier armies using [p]. *)
val avg_frontier_armies_heuristic : t -> float -> float

(** [non_frontier_armies_heuristic p num] is the heuristic score of [num] 
    non-frontier armies using [p]. *)
val non_frontier_armies_heuristic : t -> int -> float

(** [frontier_differential_heuristic p num] is the heuristic score of [num]
    frontier differential using [p]. *)
val frontier_differential_heuristic : t -> int -> float

(** [stars_heuristic p num] is the heuristic score of [num] stars using [p]. *)
val stars_heuristic : t -> int -> float

(** [opponent_num_heuristic p num] is the heuristic score of [num] opponent
    players using [p].*)
val opponent_num_heuristic : t -> int -> float

(** [max_opponent_heuristic p num] is the heuristic score of [num] where
    [num] is the maximum opponent heuristic using [p].*)
val max_opponent_heuristic : t -> float -> float

(** [avg_opponent_heuristic p num] is the heuristic score of [num] where
    [num] is the average opponent heuristic using [p]. *)
val avg_opponent_heuristic : t -> float -> float
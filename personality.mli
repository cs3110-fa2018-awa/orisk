(** Representation of the personality of the AI. *)

type t

val default : t

val node_heuristic : t -> int -> float

val bonus_heuristic : t -> int -> float

val army_heuristic : t -> int -> float

val region_heuristic : t -> int -> float

val frontier_heuristic : t -> int -> float

val frontier_armies_heuristic : t -> int -> float

val non_frontier_armies_heuristic : t -> int -> float

val frontier_differential_heuristic : t -> int -> float

val opponent_num_heuristic : t -> int -> float

val max_opponent_heuristic : t -> float -> float

val avg_opponent_heuristic : t -> float -> float

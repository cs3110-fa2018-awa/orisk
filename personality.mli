
type t

val default : t

val node_heuristic : t -> int -> float

val bonus_heuristic : t -> int -> float

val army_heuristic : t -> int -> float

val region_heuristic : t -> int -> float

val frontier_heuristic : t -> int -> float

val frontier_armies_heuristic : t -> int -> float

val frontier_differential_heuristic : t -> int -> float

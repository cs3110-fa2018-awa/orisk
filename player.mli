
type t

type color = ANSITerminal.color

val create : string -> color -> t

val player_name : t -> string

val player_color : t -> color

val compare : t -> t -> int

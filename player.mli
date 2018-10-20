
type t

type color = ANSITerminal.color

val create : string -> color -> t

val name : t -> string

val color : t -> color

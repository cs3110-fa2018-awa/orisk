
type color = ANSITerminal.color

type t = {name : string; color : color}

let create name color = {name=name; color=color}

let name ({name} : t) = name

let color ({color} : t) = color

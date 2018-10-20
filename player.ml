
type color = ANSITerminal.color

type t = {id : int; name : string; color : color}

(** [id_counter] is a unique player id. *)
let id_counter =
  let counter = ref 0
  in fun () -> incr counter; !counter

let create name color = {id=id_counter (); name=name; color=color}

let player_name ({name} : t) = name

let player_color ({color} : t) = color

let compare ({id=id} : t) ({id=id'} : t) = Pervasives.compare id id'

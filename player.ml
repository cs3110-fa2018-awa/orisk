
type color = ANSITerminal.color

(** A [Player.t] is a unique [id], a [name], and a [color]. *)
type t = {id : int; name : string; color : color}

(** [id_counter] is a unique player id. This function makes use of mutable
    state to guarantee that each invocation produces a unique ID. *)
let id_counter =
  let counter = ref 0
  in fun () -> incr counter; !counter

let create name color = {id=id_counter (); name=name; color=color}

let player_name player = player.name

let player_color player = player.color

(** [compare player1 player2] is implemented using Pervasives.compare
    and the respective IDs of each player, which are unique. This allows
    Player.t to be stored in a binary search tree. *)
let compare player1 player2 = Pervasives.compare player1.id player2.id

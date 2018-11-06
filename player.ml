(** *) (* blank doc-comment to prevent docs from confusing first one *)

(** Type alias for terminal colors. *)
type color = ANSITerminal.color

(** A [Player.t] is a unique [id], a [name], and a [color]. *)
type t = {id : int; name : string; color : color; artificial : bool}

(** [id_counter] is a unique player [id]. This function makes use of mutable
    state to guarantee that each invocation produces a unique ID. *)
let id_counter =
  let counter = ref 0
  in fun () -> incr counter; !counter

(** [create name color] is the player with [name] and [color]. *)
let create name color artificial =
  {id = id_counter (); name = name; color = color; artificial = artificial}

(** [player_id player] is the [id] of [player]. *)
let player_id player = player.id (*BISECT-IGNORE*) 
(* ignored bc eval issues due to using laziness *)

(** [player_name player] is the [name] of [player]. *)
let player_name player =
  player.name ^ if player.artificial then " [watson]" else ""

(** [player_color player] is the [color] of [player]. *)
let player_color player = player.color

let player_artificial player = player.artificial

(** [compare player1 player2] is implemented using [Pervasives.compare]
    and the respective IDs of each player, which are unique. This allows
    [Player.t] to be stored in a binary search tree. *)
let compare player1 player2 = Pervasives.compare player1.id player2.id

(** *) (* blank doc-comment to prevent docs from confusing first one *)

(** Type alias for terminal colors. *)
type color = ANSITerminal.color

(** A [Player.t] is a unique [id], a [name], a [color], and a boolean flag 
    [artifical] which indicates what kind of player. *)
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
  player.name ^ if player.artificial then " [al]" else ""

(** [player_color player] is the [color] of [player]. *)
let player_color player = player.color

(** [player_artificial player] is [true] if [player] is AI and [false] if
    [player] is human. *)
let player_artificial player = player.artificial

(** [compare player1 player2] is implemented using [Pervasives.compare]
    and the respective IDs of each player, which are unique. This allows
    [Player.t] to be stored in a binary search tree. *)
let compare player1 player2 = Pervasives.compare player1.id player2.id

open ANSITerminal
open Yojson.Basic.Util

(*BISECT-IGNORE-BEGIN*)
(** [color_of_string s] is the ANSITerminal color corresponding to [s]. *)
let color_of_string = function
  | "black" -> Black
  | "red" -> Red
  | "green" -> Green
  | "yellow" -> Yellow
  | "blue" -> Blue
  | "magenta" -> Magenta
  | "cyan" -> Cyan
  | "white" -> White
  | "default" -> Default
  | str -> failwith ("bad color str: " ^ str)

(** [string_of_color c] is the string corresponding to ANSITerminal color [c]. *)
let string_of_color = function
  | Black -> "black"
  | Red -> "red"
  | Green -> "green"
  | Yellow -> "yellow"
  | Blue -> "blue"
  | Magenta -> "magenta"
  | Cyan -> "cyan"
  | White -> "white"
  | Default -> "default"
(*BISECT-IGNORE-END*)

(** [player_of_json json] is the player that [json] represents. *)
let player_of_json json =
  {
    id = json |> member "id" |> to_int;
    name = json |> member "name" |> to_string;
    color = json |> member "color" |> to_string |> color_of_string;
    artificial = json |> member "artificial" |> to_bool
  }

(** [json_of_player player] is the JSON assoc object that represents [player]. *)
let json_of_player player =
  `Assoc [
    ("id", `Int player.id);
    ("name", `String player.name);
    ("color", `String (string_of_color player.color));
    ("artificial", `Bool player.artificial);
  ]

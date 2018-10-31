(** Dynamically draw board to terminal. *)
open ANSITerminal

(** [print_players pl] prints the name of each player in [pl] in their
    respective color. *)
val print_players : Player.t list -> unit

val draw_str : string -> int -> int -> style list -> unit

(** [draw_board gamestate] prints the board ascii with the nodes populated
    with information from the board state corresponding to [gs]. *)
val draw_board : Interface.t -> unit

(** [draw_turn gamestate] prints the current turn information based
    on [gamestate]. *)
val draw_turn : Interface.t -> unit

(** [draw_stats st] prints a leaderboard of players and their respective 
    army, territory, and continent statistics on top of the board in [st]. *)
val draw_stats : Interface.t -> unit

(** TODO *)
val pick_help : Interface.t -> string -> unit
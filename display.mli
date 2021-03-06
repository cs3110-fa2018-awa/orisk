(** Dynamically draw board to terminal. *)
open ANSITerminal

(** [width ()] is the width of the terminal screen in character cells. *)
val width : unit -> int

(** [height ()] is the height of the terminal screen in character cells. *)
val height : unit -> int

(** [print_players pl] prints the name of each player in [pl] in their
    respective color. *)
val print_players : Player.t list -> unit

(** [draw_str s x y color] prints [s] at terminal coordinates [x,y] 
    in [color]. *)
val draw_str : string -> int -> int -> style list -> unit

(** [draw_board st move] prints the board ascii with the nodes populated
    with information from the board state corresponding to [st] 
    and [move] if the current player is artificial. *)
val draw_board : Interface.t -> Move.move option -> unit

(** [draw_turn st move] prints the current turn information based
    on [st] and [move] if the current player is artificial. *)
val draw_turn : Interface.t -> Move.move option -> unit

(** [draw_stats st] prints a leaderboard of players and their respective 
    army, territory, and continent statistics on top of the board in [st]. *)
val draw_stats : Interface.t -> unit

(** [pick_help st cat] prints a help menu containing the list of possible
    actions the current player can take, given by the provided turn category
    [cat] in [st]. *)
val pick_help : Interface.t -> string -> unit
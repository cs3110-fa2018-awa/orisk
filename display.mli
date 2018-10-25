(** Dynamically draw board to terminal. *)

(** [draw_board gamestate] prints the board ascii with the nodes populated
    with information from the board state corresponding to [gs]. *)
val draw_board : Game_state.t -> unit

(** [draw_turn gamestate] prints the current turn information based
    on [gamestate]. *)
val draw_turn : Game_state.t -> unit
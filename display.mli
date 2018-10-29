(** Dynamically draw board to terminal. *)

(** [draw_board gamestate] prints the board ascii with the nodes populated
    with information from the board state corresponding to [gs]. *)
val draw_board : Interface.t -> unit

(** [draw_turn gamestate] prints the current turn information based
    on [gamestate]. *)
val draw_turn : Interface.t -> unit

(** TODO: temporary for debugging *)
val draw_stats : Interface.t -> unit
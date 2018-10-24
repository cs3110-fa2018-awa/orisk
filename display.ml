open ANSITerminal
open Board
open Board_state
open Game_state
open Player

(** TODO: make nicer ascii map for demo *)
(** TODO: make errors look nicer *)
(** TODO: display turn information *)
(** TODO: draw something nicer when node is not owned *)

(** [format_2digit num] is the 2 digit string representation of [num]. 
    Requires: 0 <= [num] <= 99 *)
let format_2digit (i : int) : string =
  if i < 10 then "0" ^ (string_of_int i) else (string_of_int i)

(** [draw_str s x y color] prints [s] at terminal coordinates [x,y] 
    in [color]. *)
let draw_str (s : string) (x : int) (y : int) (c : color) : unit =
  ANSITerminal.set_cursor x y;
  ANSITerminal.print_string [Foreground c] s

(** [draw_nodes gamestate] populates the screen with all node army values at
    their corresponding coordinates in [gamestate]. *)
let draw_nodes (gs : Game_state.t) : unit = 
  let brd_st = gs |> Game_state.board_st in
  let brd = brd_st |> Board_state.board in
  Board.fold_nodes brd 
    (fun id () ->
       (* only redraw if node is owned by a player *)
       match (Board_state.node_owner brd_st id) with
       | Some player ->
         draw_str (Board_state.node_army brd_st id |> format_2digit)
           (Board.node_coords brd id |> Board.x |> (+) 1)
           (Board.node_coords brd id |> Board.y |> (+) 1)
           (Player.player_color player)
       | None -> ()
    ) ()

(** [draw_turn gamestate] prints the current turn information based
    on [gamestate]. *)
let draw_turn (gs : Game_state.t) : unit = 
  let player = (current_player gs)
  in print_string [Foreground (player_color player)] (player_name player);
  print_string [] " -- ";
  print_string [] begin
    match turn gs with
    | Reinforce -> "Reinforce " ^ (remaining_reinforcements gs |> string_of_int)
    | Attack -> "Attack"
    | Fortify -> "Fortify"
  end;
  print_string [] "\n"

(** [draw_board gamestate] prints the board ascii with the nodes populated
    with information from the board state corresponding to [gs]. *)
let draw_board (gs : Game_state.t) : unit = 
  (* clear screen *)
  ANSITerminal.erase Screen;
  (* print topleft corner *)
  ANSITerminal.set_cursor 1 1; 
  (* print static board *)
  ANSITerminal.print_string [Foreground White] 
    (gs |> Game_state.board_st |> Board_state.board |> Board.board_ascii);
  (* populate nodes *)
  draw_nodes gs;
  (* move to bottom of board *)
  set_cursor 0 (gs |> board_st |> board |> board_ascii_height);
  print_string [] "\n";
  (* print out current turn information *)
  draw_turn gs

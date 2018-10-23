open ANSITerminal
open Board
open Board_state
open Game_state
open Player

(** [draw_str s x y color] prints [s] at terminal coordinates [x,y] 
    in [color]. *)
let draw_str (s : string) (x : int) (y : int) (c : color) : unit =
  ANSITerminal.set_cursor x y;
  ANSITerminal.print_string [Foreground c] s

let pad_zero n i =
  let str = (string_of_int i)
  in let rem = n - (String.length str)
  in if rem > 0 then (String.make rem '0') ^ str else str

(** [draw_nodes gamestate] populates the screen with all node army values at
    their corresponding coordinates in [gamestate]. *)
let draw_nodes (gs : Game_state.t) : unit = 
  let brd_st = gs |> Game_state.board_st in
  let brd = brd_st |> Board_state.board in
  Board.fold_nodes brd 
    (fun id () -> 
       draw_str (Board_state.node_army brd_st id |> pad_zero 2)
         (* times two because a map coordinate is 2x1 not 1x1*)
         (Board.node_coords brd id |> Board.x |> ( * ) 2 |> (+) 1)
         (Board.node_coords brd id |> Board.y |> (+) 1)
         (match (Board_state.node_owner brd_st id) with 
          | Some p -> Player.player_color p
          | None -> failwith "Player not found")
    ) ()

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
  let player = (current_player gs)
  in print_string [Foreground (player_color player)] (player_name player);
  print_string [] " -- ";
  print_string [] begin
    match turn gs with
    | Reinforce -> "Reinforce " ^ (remaining_reinforcements gs |> string_of_int)
    | Attack -> "Attack"
  end;
  print_string [] "\n";

open ANSITerminal
open Board
open Board_state
open Game_state
open Player
open Interface

(** TODO *)
let header_len = String.length "Player"

(** constant*)
let spacing = 3

(** constant *)
let spacing_head = 6 (* (n)ode *)

(** [format_2digit num] is the 2 digit string representation of [num]. 
    Requires: 0 <= [num] <= 99 *)
let format_2digit (i : int) : string =
  if i < 10 then "0" ^ (string_of_int i) else (string_of_int i)

(** [draw_str s x y color] prints [s] at terminal coordinates [x,y] 
    in [color]. *)
let draw_str (s : string) (x : int) (y : int) (f : style list) : unit =
  ANSITerminal.set_cursor x y;
  ANSITerminal.print_string f s

(** TODO *)
let max_column_len (gs : Game_state.t) : int =
  let name_len = 
    List.fold_left (fun acc p -> max acc (p |> Player.player_name |> String.length))
      0 (gs |> Game_state.board_st |> Board_state.get_players) in 
  max header_len name_len

(** TODO *)
let make_n_spaces (n:int) : string =
  let rec h n acc =
    if n > 0 then h (n-1) (acc ^ " ") else acc in 
  h n ""

(** TODO *)
let column_spacing (s:string) (c_len:int) : string =
  make_n_spaces (c_len - String.length s)

(** TODO: printing with proper headers *)
let draw_stats (st : Interface.t) =
  let gs = Interface.game_state st in
  let col_len = max_column_len gs + spacing in
  let header = "Player" ^ make_n_spaces (col_len - 6) ^ "(a)rmy" ^
               make_n_spaces spacing ^ "(n)ode" ^ make_n_spaces spacing ^
               "(c)ont" ^ make_n_spaces spacing in
  let draw_one_line (ps : Board_state.player_stats) : unit =
    match ps with
    | {player=p; army_tot=a; node_tot=n; cont_tot=c} -> 
      let name = p |> Player.player_name in
      print_string [] "| ";
      print_string [Foreground (player_color p)] (player_name p);
      print_endline (column_spacing name col_len ^ (string_of_int a) ^ 
                     column_spacing (string_of_int a) (spacing+spacing_head) ^ (string_of_int n) ^
                     column_spacing (string_of_int n) (spacing+spacing_head) ^ (string_of_int c) ^
                     column_spacing (string_of_int c) (spacing+spacing_head) ^ " |") in 
  let rec draw_all (ps : Board_state.player_stats list) : unit =
    match ps with
    | [] -> ()
    | hd :: tl -> draw_one_line hd; draw_all tl in
  ANSITerminal.set_cursor 1 2; (* to account for the space in the ascii json *) 
  print_string [Bold] "------------------------------------------\n";
  print_string [Bold] ("| " ^ header ^ " |\n");
  print_string [Bold] "------------------------------------------\n";
  draw_all (Board_state.sorted_player_stats (leaderboard_cat st) (Game_state.board_st gs));
  print_string [Bold] "------------------------------------------\n";
  (* to account for the extra turn information being drawn? temporary hard code *)
  ANSITerminal.set_cursor 0 (game_state st |> board_st |> Board_state.board |> board_ascii_height |> (+) 2) 

(** [draw_nodes gamestate] populates the screen with all node army values at
    their corresponding coordinates in [gamestate]. *)
let draw_nodes (st : Interface.t) : unit = 
  let brd_st = game_state st |> Game_state.board_st in
  let brd = brd_st |> Board_state.board in
  Board.fold_nodes brd 
    (fun id () ->
       (* only redraw if node is owned by a player *)
       let x = Board.node_coords brd id |> Board.x
       in let y = Board.node_coords brd id |> Board.y
       in let is_cursor = cursor_node st = id
       in let is_selected = 
            from_fortify_node st = Some id || attacking_node st = Some id
       in let style = 
            match (Board_state.node_owner brd_st id),is_cursor,is_selected with
            | None,true,_ -> [Foreground Black; Background White]
            | None,false,_ -> [Foreground White;Background Black]
            | Some player,false,false 
              -> [Foreground (player_color player);Background Black]
            | Some player,false,true 
              -> [Foreground (player_color player);Background White]
            | Some player,true,_ 
              -> [Foreground White;Background (player_color player)]
       in draw_str (Board_state.node_army brd_st id |> format_2digit)
         (x + 1) (y + 1) style 
    ) ()

(** [draw_turn gamestate] prints the current turn information based
    on [gamestate]. *)
let draw_turn (st : Interface.t) : unit = 
  let player = game_state st |> current_player
  in print_string [Foreground (player_color player)] (player_name player);
  print_string [] " -- ";
  print_string [] (turn_to_str (game_state st));
  print_string [] "\n"

(** [draw_board gamestate] prints the board ascii with the nodes populated
    with information from the board state corresponding to [gamestate]. *)
let draw_board (st : Interface.t) : unit = 
  (* clear screen *)
  ANSITerminal.erase Screen;
  (* print topleft corner *)
  ANSITerminal.set_cursor 1 1; 
  (* print static board *)
  ANSITerminal.print_string [Foreground White] 
    (game_state st |> Game_state.board_st |> Board_state.board |> Board.board_ascii);
  (* populate nodes *)
  draw_nodes st;
  (* move to bottom of board *)
  set_cursor 0 (game_state st |> board_st |> Board_state.board |> board_ascii_height);
  print_string [] "\n";
  (* print out current turn information *)
  draw_turn st

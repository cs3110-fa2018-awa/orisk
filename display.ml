open ANSITerminal
open Board
open Board_state
open Game_state
open Player
open Interface

(** [format_2digit num] is the 2 digit string representation of [num]. 
    Requires: 0 <= [num] <= 99 *)
let format_2digit (i : int) : string =
  if i < 10 then "0" ^ (string_of_int i) else (string_of_int i)

(** [print_players pl] prints the name of each player in [pl] in their
    respective color. *)
let rec print_players (pl : Player.t list) : unit =
  match pl with
  | [] -> ()
  | player :: rest -> 
    print_string [Foreground (player_color player)] (player_name player ^ "\n");
    print_players rest

(** [draw_str s x y color] prints [s] at terminal coordinates [x,y] 
    in [color]. *)
let draw_str (s : string) (x : int) (y : int) (f : style list) : unit =
  ANSITerminal.set_cursor x y;
  ANSITerminal.print_string f s

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

(* leaderboard functions ---------------------------------------------------- *)

(** [header_len_fst] is the length of the first column header in the leaderboard.
    Used as an internal constant. *)
let header_len_fst = String.length "(p)layer"

(** [header_len] is the length of the other column headers in the leaderboard.
    The headers are (a)rmy, (n)ode, and (c)ont.
    Used as an internal constant. *)
let header_len = 6 

(** [spacing] is the number of spaces between columns in the leaderboard. 
    Used as an internal constant. *)
let spacing = 3

(** [column_max_len gamestate] is either the length of the first header in
    the leaderboard or the length of the longest player name in [gamestate]. *)
let column_max_len (gs : Game_state.t) : int =
  let name_len = 
    List.fold_left (fun acc p -> max acc (p |> Player.player_name |> String.length))
      0 (gs |> Game_state.board_st |> Board_state.get_players) in 
  max header_len_fst name_len

(** [make_n_chars n s] is the string created by repeating [s] [n] times. 
    Example: [make_n_chars 5 "o"] is "ooooo". *)
let make_n_chars (n : int) (s : string) : string =
  let rec h n acc =
    if n > 0 then h (n-1) (acc ^ s) else acc in 
  h n ""

(** [column_spacing text column_length] is the string spacing created for padding
    after [text] in a column of [column_length]. *)
let column_spacing (s : string) (col_len : int) : string =
  make_n_chars (col_len - String.length s) " "

(** [centered_x_coord board_width leaderboard_width] is the x coordinate that,
    if drawn at, will result in a horizontally centered leaderboard. *)
let centered_x_coord board_width leaderboard_width = 
  board_width / 2 - leaderboard_width / 2

(** [centered_y_coord board_width leaderboard_width] is the y coordinate that,
    if drawn at, will result in a vertically centered leaderboard. *)
let centered_y_coord board_height leaderboard_height =
  (* +1 to account for newline at beginning of board ascii *)
  board_height / 2 - (leaderboard_height / 2) + 1

(** [draw_stats st] draws a leaderboard of players and their respective 
    army, territory, and continent statistics on top of the board in [st]. *)
let draw_stats (st : Interface.t) =
  let gs = Interface.game_state st in
  let brd = st |> board in
  let col_len = column_max_len gs + spacing in
  let header = "| " ^ "(p)layer" ^ make_n_chars (col_len - header_len_fst) " " ^
               "(a)rmy" ^ make_n_chars spacing " " ^ "(n)ode" ^
               make_n_chars spacing " " ^ "(c)ont" ^ make_n_chars spacing " " ^ " |" in
  let divider = make_n_chars (String.length header) "-" in
  let leaderboard_height = List.length (gs |> board_st |> get_players) + 4 in
  (* [set_cursor_y_incr] handles drawing each line at the correct height *)
  let set_cursor_y_incr = 
    let counter = ref (centered_y_coord (board_ascii_height brd) leaderboard_height)
    in fun () -> incr counter; !counter in
  let draw_one_line (ps : Board_state.player_stats) : unit =
    match ps with
    | {player=p; army_tot=a; node_tot=n; cont_tot=c} -> 
      let name = p |> Player.player_name in
      let total_col_space = header_len + spacing in
      ANSITerminal.set_cursor (centered_x_coord (board_ascii_width brd)
                                 (String.length header)) (set_cursor_y_incr ());
      print_string [] "| ";
      print_string [Foreground (player_color p)] (player_name p);
      print_endline (column_spacing name col_len ^ (string_of_int a) ^ 
                     column_spacing (string_of_int a) (total_col_space) ^(string_of_int n) ^
                     column_spacing (string_of_int n) (total_col_space) ^ (string_of_int c) ^
                     column_spacing (string_of_int c) (total_col_space) ^ " |") in 
  let rec draw_all (ps : Board_state.player_stats list) : unit =
    match ps with
    | [] -> ()
    | hd :: tl -> draw_one_line hd; draw_all tl in
  draw_str (divider ^ "\n") (centered_x_coord (board_ascii_width brd)
                               (String.length header)) (set_cursor_y_incr ()) [Bold];
  draw_str (header ^ "\n") (centered_x_coord (board_ascii_width brd)
                              (String.length header)) (set_cursor_y_incr ()) [Bold];
  draw_str (divider ^ "\n") (centered_x_coord (board_ascii_width brd)
                               (String.length header)) (set_cursor_y_incr ()) [Bold];
  draw_all (Board_state.sorted_player_stats (leaderboard_cat st) (Game_state.board_st gs));
  draw_str (divider ^ "\n") (centered_x_coord (board_ascii_width brd)
                               (String.length header)) (set_cursor_y_incr ()) [Bold];
  (* +2 to account for the extra turn information being drawn *)
  ANSITerminal.set_cursor 0
    (game_state st |> board_st |> Board_state.board |> board_ascii_height |> (+)2) 

(* help functions ---------------------------------------------------- *)

(** [help_str_pick] is the string containing information about the 
    possible player commands valid during the territory initialization phase .*)
let help_str_pick = [
  "[arrow keys]  navigate the board\n";
  "[ ], [enter]  select territory\n";
  "[a-z 0-9]     search the board\n";
  "[`]           populate the board territories randomly\n";
  "[-]           toggle help sidebar";
  "[esc]         quit game\n"
]

(** [help_str_leaderboard] is the string containing information about the 
    possible player commands valid during when the leaderboard is on. *)
let help_str_leaderboard = [
  "[p]    sort by player\n"; 
  "[a]    sort by army count\n";
  "[n]    sort by territory count\n";
  "[c]    sort by continent count\n";
  "[=]    untoggle leaderboard\n";
  "[-]    toggle help sidebar";
  "[esc]  quit game\n"
]

(** [help_str_game] is the string containing information about the 
    possible player commands valid during the main game.*)
let help_str_game = [
  "[arrow keys]  navigate the board\n";
  "[ ], [enter]  confirm action\n";
  "[?]           end turn\n";
  "[\\]           cancel current action\n";
  "[tab]         cycle through relevant nodes\n";
  "[a-z 0-9]     search the board\n";
  "[=]           toggle leaderboard\n";
  "[-]           toggle help sidebar";
  "[esc]         quit game\n"
]

(** [draw_help st] prints the list of possible actions the current player can
    take, given by [cat]. This list will be printed to the right of the board
    ascii art corresponding to [st]. *)
let draw_help (st : Interface.t) (cat : string list) : unit =
  let set_cursor_y = 
    let counter = ref 1 (* draw at top of board *)
    in fun () -> incr counter; !counter
  in let rec internal = function
      | [] -> ()
      | s :: rest -> 
        draw_str s (board_ascii_width (board st) + 5) (set_cursor_y ()) [];
        internal rest
  in internal cat;
  (* +2 to account for the extra turn information being drawn *)
  ANSITerminal.set_cursor 0
    (game_state st |> board_st |> Board_state.board |> board_ascii_height |> (+)2) 

(** [pick_help st cat] prints a help menu containing the list of possible
    actions the current player can take, given by the provided status [cat] in
    [st]. *)
let pick_help (st : Interface.t ) = function
  | "pick" -> draw_help st help_str_pick
  | "leaderboard" -> draw_help st help_str_leaderboard
  | "game" -> draw_help st help_str_game
  | _ -> failwith "invalid category"
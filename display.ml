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

(** [width ()] is the width of the terminal screen in character cells. *)
let width () = size () |> fst

(** [height ()] is the height of the terminal screen in character cells. *)
let height () = size () |> snd

(** [node_style st id] is the style that node [id] should be drawn with
    in state [st]. *)
let node_style st id =
  let brd_st = game_state st |> Game_state.board_st
  in let is_cursor = cursor_node st = id
  in let is_selected = 
       from_fortify_node st = Some id || attacking_node st = Some id
  in match (Board_state.node_owner brd_st id),is_cursor,is_selected with
  | None,true,_ -> [Foreground Black; Background White]
  | None,false,_ -> [Foreground White;Background Black]
  | Some player,false,false 
    -> [Foreground (player_color player);Background Black]
  | Some player,false,true 
    -> [Foreground (player_color player);Background White]
  | Some player,true,_ 
    -> [Foreground White;Background (player_color player)]

(** [handle_scroll_edges str x y scrollx scrolly] is the tuple of cropped
    string and starting index that [str] should be drawn with given
    position [x] and [y] and scroll offsets [scrollx] and [scrolly]. *)
let handle_scroll_edges str x y scrollx scrolly : string * int =
  if x >= scrollx && x < width () + scrollx - 1
  then str,0
  else if x = width () + scrollx - 1
  then String.sub str 0 1,0
  else if x = scrollx - 1
  then String.sub str 1 1,1
  else "",0

(** [draw_nodes gamestate] populates the screen with all node army values at
    their corresponding coordinates in [gamestate]. *)
let draw_nodes (st : Interface.t) : unit =
  let scrollx, scrolly = scroll st in
  let brd_st = game_state st |> Game_state.board_st in
  let brd = brd_st |> Board_state.board in
  (* draw all nodes *)
  Board.fold_nodes brd 
    (fun id () ->
       let x = Board.node_coords brd id |> Board.x
       in let y = Board.node_coords brd id |> Board.y
       (* determine style *)
       in let style = node_style st id
       in let str = Board_state.node_army brd_st id |> format_2digit
       (* calculate values for handling corner cases from edges and scrolling *)
       in let crop,x_off = handle_scroll_edges str x y scrollx scrolly
       in begin
         (* handle different cases for edges and scrolling *)
         if y - 1 >= scrolly && y - 1 < height () + scrolly - 3
         then draw_str crop (x - scrollx + 1 + x_off) (y - scrolly) style
         else ()
       end
    ) ()

(** [draw_turn gamestate] prints the current turn information based
    on [gamestate]. *)
let draw_turn (st : Interface.t) : unit = 
  let player = game_state st |> current_player
  in print_string [Foreground (player_color player)] (player_name player);
  print_string [] " -- ";
  print_string [] (turn_to_str (game_state st));
  print_string [] " -- ";
  (* print_string [] (Al.best_move (game_state st) 0 |> Move.string_of_move);*)
  print_string [] "\n"

let draw_line st num line : int =
  let scrollx, scrolly = scroll st
  in let disp = 
       String.sub line scrollx (min (width ()) (String.length line - scrollx))
  in if num >= scrolly && num < height () + scrolly - 3
  then begin print_string [white] (disp ^ "\n"); num + 1 end else num + 1

(** [draw_board gamestate] prints the board ascii with the nodes populated
    with information from the board state corresponding to [gamestate]. *)
let draw_board (st : Interface.t) : unit = 
  (* clear screen *)
  ANSITerminal.erase Screen;
  (* print topleft corner *)
  ANSITerminal.set_cursor 1 1; 
  (* print static board *)
  ignore (List.fold_left (draw_line st) 0 (st |> board |> board_ascii_lines));
  (* populate nodes *)
  draw_nodes st;
  (* move to bottom of board *)
  set_cursor 0 (min (game_state st |> board_st |> Board_state.board 
                     |> board_ascii_height) (height () - 3));
  print_string [] "\n";
  (* print out current turn information *)
  draw_turn st

(* leaderboard functions ---------------------------------------------------- *)

(** [header_len_fst] is the length of the first column header in the 
    leaderboard. Used as an internal constant. *)
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
    List.fold_left 
      (fun acc p -> max acc (p |> Player.player_name |> String.length))
      0 (gs |> Game_state.board_st |> Board_state.get_players)
  in max header_len_fst name_len

(** [make_n_chars n s] is the string created by repeating [s] [n] times. 
    Example: [make_n_chars 5 "o"] is "ooooo". *)
let make_n_chars (n : int) (s : string) : string =
  let rec h n acc =
    if n > 0 then h (n-1) (acc ^ s) else acc in 
  h n ""

(** [column_spacing text column_length] is the string spacing created for 
    padding after [text] in a column of [column_length]. *)
let column_spacing (s : string) (col_len : int) : string =
  make_n_chars (col_len - String.length s) " "

(** [centered_x_coord board_width leaderboard_width] is the x coordinate that,
    if drawn at, will result in a horizontally centered leaderboard. *)
let centered_x_coord board_width leaderboard_width = 
  let terminal_width = width () in
  min (terminal_width / 2 - leaderboard_width / 2)
    (board_width / 2 - leaderboard_width / 2)

(** [centered_y_coord board_width leaderboard_width] is the y coordinate that,
    if drawn at, will result in a vertically centered leaderboard. *)
let centered_y_coord board_height leaderboard_height =
  let terminal_height = height () in 
  min ((terminal_height - 3) / 2 - (leaderboard_height / 2) + 1)
    (board_height / 2 - (leaderboard_height / 2) + 1)

let stats_draw_one_line (ps : Board_state.player_stats)
    gs brd header set_cursor_y_incr col_len : unit =
  match ps with
  | {player=p; army_tot=a; node_tot=n; cont_tot=c; star_tot=s} -> 
    let name = p |> Player.player_name in
    let total_col_space = header_len + spacing in
    ANSITerminal.set_cursor (centered_x_coord (board_ascii_width brd)
                               (String.length header)) (set_cursor_y_incr ());
    print_string [] "| ";
    print_string [Foreground (player_color p)] (player_name p);
    print_endline (column_spacing name col_len ^ (string_of_int a) ^ 
                   column_spacing (string_of_int a) (total_col_space) ^ 
                   (string_of_int n) ^
                   column_spacing (string_of_int n) (total_col_space) ^ 
                   (string_of_int c) ^
                   column_spacing (string_of_int c) (total_col_space) ^ 
                   (string_of_int s) ^ 
                   column_spacing (string_of_int s) (total_col_space) ^ " |")

(** [draw_stats st] draws a leaderboard of players and their respective 
    army, territory, continent, and star statistics on top of the board
    in [st]. *)
let draw_stats (st : Interface.t) =
  let gs = Interface.game_state st in
  let brd = st |> board in
  let col_len = column_max_len gs + spacing in
  let header = "| " ^ "(p)layer" ^ make_n_chars (col_len - header_len_fst) " " ^
               "(a)rmy" ^ make_n_chars spacing " " ^ "(n)ode" ^
               make_n_chars spacing " " ^ "(c)ont" ^ make_n_chars spacing " " ^
               "(s)tar" ^ make_n_chars spacing " " ^ " |" in
  let divider = make_n_chars (String.length header) "-" in
  let leaderboard_height = List.length (gs |> board_st |> get_players) + 4 in
  (* [set_cursor_y_incr] handles drawing each line at the correct height *)
  let set_cursor_y_incr = 
    let counter = 
      ref (centered_y_coord (board_ascii_height brd) leaderboard_height)
    in fun () -> incr counter; !counter in
  let rec draw_all (ps : Board_state.player_stats list) : unit =
    match ps with
    | [] -> ()
    | hd :: tl ->
      stats_draw_one_line hd gs brd header set_cursor_y_incr col_len;
      draw_all tl in
  draw_str (divider ^ "\n")
    (centered_x_coord (board_ascii_width brd)
       (String.length header)) (set_cursor_y_incr ()) [Bold];
  draw_str (header ^ "\n")
    (centered_x_coord (board_ascii_width brd)
       (String.length header)) (set_cursor_y_incr ()) [Bold];
  draw_str (divider ^ "\n")
    (centered_x_coord (board_ascii_width brd)
       (String.length header)) (set_cursor_y_incr ()) [Bold];
  draw_all (Board_state.sorted_player_stats (leaderboard_cat st) 
              (Game_state.board_st gs));
  draw_str (divider ^ "\n")
    (centered_x_coord (board_ascii_width brd)
       (String.length header)) (set_cursor_y_incr ()) [Bold];
  set_cursor 0 
    (min (game_state st |> board_st |> Board_state.board |> board_ascii_height 
          |> (+) 3) (height ()))

(* help functions ---------------------------------------------------- *)

(** [help_str_pick] is the string containing information about the 
    possible player inputs valid during the territory initialization phase .*)
let help_str_pick = [
  "[arrow keys]  navigate the board";
  "[ ], [enter]  select territory";
  "[tab]         cycle through nodes";
  "[a-z 0-9]     search the board";
  "[`]           populate the board territories randomly";
  "[.]           save game";
  "[-]           toggle help";
  "[esc]         quit game"
]

(** [help_str_leaderboard] is the string containing information about the 
    possible player inputs valid during when the leaderboard is on. *)
let help_str_leaderboard = [
  "[p]    sort by player"; 
  "[a]    sort by army count";
  "[n]    sort by territory count";
  "[c]    sort by continent count";
  "[=]    untoggle leaderboard";
  "[-]    toggle help";
  "[esc]  quit game"
]

(** [help_str_game] is the string containing information about the 
    possible player inputs valid during the main game.*)
let help_str_game = [
  "[arrow keys]  navigate the board";
  "[ ], [enter]  confirm action";
  "[?]           end turn";
  "[\\]           cancel current action";
  "[tab]         cycle through relevant nodes";
  "[a-z 0-9]     search the board";
  "[=]           toggle leaderboard";
  "[.]           save game";
  "[-]           toggle help";
  "[esc]         quit game";
  "";
  "Stars to armies:";
  "1 star  :  1 army";
  "2 stars :  2 armies";
  "3 stars :  4 armies";
  "4 stars :  7 armies";
  "5 stars : 10 armies"
]

(** [max_help_str] is the length of the longest line in [s]. *)
let max_help_str s =
  List.fold_left (fun acc s -> max acc (String.length s)) 0 s

(** [draw_help st] prints the list of possible actions the current player can
    take, given by help string [s]. This list will be printed on top of the
    ascii art corresponding to [st]. *)
let draw_help (st : Interface.t) (s : string list) : unit =
  let brd = st |> board in
  let col_len = max_help_str s in
  let divider = make_n_chars (col_len + 4) "-" in
  let help_height = (List.length s) + 2 in
  let cursor_y_incr = 
    let counter = ref (centered_y_coord (board_ascii_height brd) help_height)
    in fun () -> incr counter; !counter in
  let draw_one_line (str : string) = 
    ANSITerminal.set_cursor (centered_x_coord (board_ascii_width brd)
                               (String.length divider)) (cursor_y_incr ());
    print_string [] ("| " ^ str ^ (column_spacing str col_len) ^ " |\n") in
  let rec draw_all = function 
    | [] -> () 
    | hd :: tl -> draw_one_line hd; draw_all tl in 
  (* drawing the help box*)
  draw_str (divider ^ "\n")
    (centered_x_coord (board_ascii_width brd)
       (String.length divider)) (cursor_y_incr ()) [Bold];
  draw_all s;
  draw_str (divider ^ "\n")
    (centered_x_coord (board_ascii_width brd)
       (String.length divider)) (cursor_y_incr ()) [Bold];
  set_cursor 0 
    (min (game_state st |> board_st |> Board_state.board |> board_ascii_height 
          |> (+) 3) (height ()))

(** [pick_help st cat] prints a help menu containing the list of possible
    actions the current player can take, given by the provided turn category
    [cat] in [st]. *)
let pick_help (st : Interface.t) = function
  | "pick" -> draw_help st help_str_pick
  | "leaderboard" -> draw_help st help_str_leaderboard
  | "game" -> draw_help st help_str_game
  | _ -> failwith "invalid category"

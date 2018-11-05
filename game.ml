open Game_state
open Board
open Board_state
open Display
open Player
open Interface

(** [string_of_dice dice] is a string of the list [dice]. *)
let string_of_dice dice =
  (** [internal acc d] recursively concatenates the elements of [d]. 
      The elements of [d] represent the results of rolling a random 
      six-sided die. 

      Helper for [string_of_dice].*)
  let rec internal acc = function 
    | [] -> acc ^ "]"
    | hd :: ((nxt :: nxt2) as tl) 
      -> internal (acc ^ (string_of_int (hd + 1)) ^ ", ") tl
    | hd :: tl -> internal (acc ^ (string_of_int (hd + 1))) tl
  in internal "[" dice

(** [win_yet st] checks if any player in [st] satisfies the win condition. 
    Prints the [name] of the winning [player] if there is one. 

    Win condition is controlling the entire map i.e. all the continents or
    territories. *)
let win_yet (st:Game_state.t) : unit =
  (** [check l] checks every [player] in [l] to see if anyone satisfies the
      win condition specified in [win_yet]. 

      Helper for [win_yet]. *) 
  let rec check (l: Player.t list) =
    match l with
    | [] -> ()
    | p :: rest -> 
      (* check if a player owns all possible continents yet *)
      if (p |> Board_state.player_conts (st |> Game_state.board_st) 
          |> List.length) =
         (st |> Game_state.board_st |> Board_state.board |> Board.conts 
          |> List.length)
      then (ANSITerminal.print_string [Foreground (player_color p)] 
              ((player_name p)^" wins!\n"); ignore (exit 0)) 
      else ()
  in check (st |> Game_state.players)

(** [next_valid_node st] is the next node that is valid for the player to select 
    according to the current turn state of [st]. *)
let next_valid_node st =
  let node = cursor_node st
  in let lst = turn_valid_nodes st
  in let rec helper = function
      | hd :: next :: tl when hd = node -> next
      | hd :: [] when hd = node -> List.hd lst
      | [] -> List.hd lst
      | hd :: tl -> helper tl
  in if List.length lst = 0 then None else Some (helper lst)

(** [read_input ()] reads a key input into a string without waiting 
    for a return key. 

    Implementation was inspired by https://stackoverflow.com/a/13410456. *)
let read_input () =
  let buf = Bytes.create 8
  in let termio = Unix.tcgetattr Unix.stdin
  in let () = Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
         {termio with c_icanon = false; c_echo = false}
  in let len = input stdin buf 0 8
  in let () = Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio
  in Bytes.sub_string buf 0 len

(** [char_regexp] is the regular expression for valid characters when searching 
    for a node. *)
let char_regexp = Str.regexp "[A-Za-z0-9]"

(** [int_regexp] is the regular expression for valid characters when inputting
    an integer. *)
let int_regexp = Str.regexp "[0-9]"

(** [read_num str prev] is [Some s] where [s] is the string entered by the user
    before pressing either the spacebar or return key and [None] if the user
    inputs "?"" or "\\"". 

    This prevents users from entering anything except 
    integers when [read_num] is applied. However, they will still be able to 
    quit and rewind the turn. *)
let rec read_num str prev : string option =
  ANSITerminal.move_cursor (- (String.length prev)) 0;
  ANSITerminal.erase Eol;
  print_string str;
  flush stdout;
  match read_input () with
  | c when Str.string_match int_regexp c 0 -> read_num (str ^ c) str
  | "\127" -> if String.length str <= 0 then read_num str str
    else read_num (String.sub str 0 (String.length str - 1)) str
  | "\004" | "\027" -> print_endline("\nThanks for playing!\n"); exit 0
  | " " | "\n" -> Some str
  | "?" | "\\" -> None
  | _ -> read_num str str

(** [game_stage st] is [(st',x)] where [st'] is the new state from evaluating
    the turn state of [st] and [x] is [Some msg] or [None]. *)
let game_stage st = match st |> game_state |> turn with 
  | Pick _ -> pick st,None
  | Trade -> failwith "sanity check"
  | Reinforce (SelectR,_) -> reinforce_place st (Some (cursor_node st)),None
  | Reinforce (PlaceR node,remaining) -> failwith ";-;"
  | Attack AttackSelectA -> Some (cursor_node st) |> change_attack_node st,None
  | Attack (DefendSelectA node)
    -> let gst',attack,defend = attack (game_state st) node (cursor_node st) 
           (min ((node_army (board_state st) node) - 1) 3)
    in begin match turn gst' with
      | Attack (OccupyA _) -> change_game_st st gst'
      | _ -> change_game_st (set_cursor_node st (attacking_node st)) gst'
    end
     , (Some ("A: " ^ (string_of_dice attack) 
              ^ " vs D: " ^ (string_of_dice defend))) 
  | Attack (OccupyA (n1,n2)) -> failwith "shouldn't happen"
  | Fortify FromSelectF 
    -> Some (cursor_node st) |> change_from_fortify_node st,None
  | Fortify (ToSelectF node) 
    -> fortify_select st (Some node) (Some (cursor_node st)),None
  | Fortify (CountF (n1,n2)) -> failwith "):"

(** [game_nums st num] is [(st',x)] where [st'] is the new state from evaluating
    the turn state of [st] using [num] and [x] is [Some msg] or [None]. *)
let game_nums st num = match st |> game_state |> turn with 
  | Trade
    -> trade_stars (game_state st) num |> change_game_st st, None
  | Reinforce ((PlaceR node),_) 
    -> change_game_st st (reinforce (game_state st) (cursor_node st) num),None
  | Attack (OccupyA (n1,n2)) 
    -> change_game_st st (occupy (game_state st) n1 n2 num),None
  | Fortify (CountF (n1,n2)) 
    -> fortify (game_state st) n1 n2 num |> change_game_st st,None
  | _ -> failwith "shouldnt happen"

(** [perform_search st str] is the (string * bool) tuple containing the
    search string and the success flag resulting from performing a search
    for [str] in [st]. *)
let perform_search st str : (string * bool) =
  let found_node = node_search (st |> Interface.board) str in
  str,found_node <> None

(** [parse_standard_input st msg search] is the tuple (st', msg', search')
    resulting from parsing a single character of input. Called internally
    from [game_loop_new]. *)
let parse_standard_input st msg search =
  if (help_on st)
  then begin match read_input () with
    | "-" -> (toggle_help st), msg, None
    | "\004" | "\027" -> print_endline("\nThanks for playing!\n"); exit 0
    | _ -> st, msg, None
  end
  else if (Interface.leaderboard_on st)
  then begin match read_input () with
    | "=" -> (toggle_leaderboard st), msg, None
    | "p" -> (set_leaderboard_cat st CatPlayer), msg, None
    | "a" -> (set_leaderboard_cat st CatArmy), msg, None
    | "n" -> (set_leaderboard_cat st CatNode), msg, None
    | "c" -> (set_leaderboard_cat st CatCont), msg, None
    | "-" -> (toggle_help st), msg, None
    | "\004" | "\027" -> print_endline("\nThanks for playing!\n"); exit 0
    | _ -> st, msg, None
  end
  else begin match read_input () with
    | "\027[A" -> (move_arrow st Up), msg, None
    | "\027[D" -> (move_arrow st Left), msg, None
    | "\027[B" -> (move_arrow st Down), msg, None
    | "\027[C" -> (move_arrow st Right), msg, None
    (* : and ; because macs are inferior *)
    | "\027[1;2A" | ":" -> (scroll_by st 0 (-1)), msg, None
    | "\027[1;2D" -> (scroll_by st (-1) 0), msg, None
    | "\027[1;2B" | ";" -> (scroll_by st 0 1), msg, None
    | "\027[1;2C" -> (scroll_by st 1 0), msg, None
    | " " | "\n" -> let st',msg' = game_stage st in st', msg', None
    | "?" -> (change_game_st st (game_state st |> end_turn_step)), msg, None
    | "=" -> (toggle_leaderboard st), msg, None
    | "-" -> (toggle_help st), msg, None
    | "\t" -> (set_cursor_node st (next_valid_node st)), msg, None
    | "\\" -> (change_game_st st (game_state st |> back_turn)), msg, None
    | "\004" | "\027" -> print_endline("\nThanks for playing!\n"); exit 0
    | c when Str.string_match char_regexp c 0
      -> st, msg, Some (perform_search st ((fst search) ^ c))
    | "\127" -> if String.length (fst search) <= 1
      then st, msg, None
      else st, msg, Some
             (perform_search st (String.sub (fst search) 0
                                   (String.length (fst search) - 1)))
    | "`" -> if st |> game_state |> is_pick
      then (game_state st |> assign_random_nodes
            |> change_game_st st), msg, None
      else st, msg, None
    | _ -> st, msg, None
  end

(** [parse_standard_input st msg search] is the tuple (st', msg', search')
    resulting from parsing numerical input. Called internally from
    [game_loop_new]. *)
let parse_num_input st msg search =
  let (min, max, default) = st |> game_state |> min_max_default in
  print_string ("min " ^ (string_of_int min) ^
                ", max " ^ (string_of_int max) ^
                ", default: " ^ (string_of_int default) ^ " > "); 
  let handle num =
    let st',msg' = game_nums st num in
    st', msg', None in
  match read_num "" "" with
  | Some str when str = "" -> handle default
  | Some str -> handle (int_of_string str)
  | None -> (change_game_st st (game_state st |> back_turn)), msg, None

(** [parse_input st msg search] is the tuple (st' msg' search')
    resulting from parsing input, depending on the current state [st].
    Called internally from [game_loop_new]. This function handles
    exceptions and produces new messages accordingly. *)
let parse_input st msg search :
  (Interface.t * string option * (string * bool) option) =
  try 
    begin
      match game_state st |> turn with 
      | Pick _
      | Reinforce (SelectR,_) 
      | Attack (AttackSelectA | DefendSelectA _) 
      | Fortify (FromSelectF | ToSelectF _)
        (* these states have standard input *)
        -> parse_standard_input st msg search
      | Trade | Reinforce (PlaceR _,_) | Attack (OccupyA _) | Fortify (CountF _)
        (* these states have numerical input *)
        -> parse_num_input st msg search
    end
  (* handle exceptions *)
  with
  | NoPlayers
    -> st, (Some "No players!"), None
  | NonadjacentNode (node_id1,node_id2)
    -> st, (Some (node_id1 ^ " is not adjacent to " ^ node_id2 ^ "!")), None
  | NonconnectedNode (node_id1,node_id2)
    -> st, (Some (node_id1 ^ " is not connected to " ^ node_id2 ^ "!")), None
  | SameNode node_id
    -> st, (Some ("You can't perform this action on the same territory!")), None
  | InvalidState (turn_state)
    -> st, (Some "Wrong type of turn."), None
  | InsufficientArmies (node_id,army)
    -> st, (Some ("You only have " ^ (string_of_int army) ^
                  " armies to attack with! You can't attack from " ^
                  node_id ^ "!")), None
  | InsufficientStars s
    -> st, (Some ("You don't have " ^ string_of_int s ^ " stars!")), None
  | FriendlyFire player
    -> st, (Some "You can't attack yourself!"), None
  | UnknownNode n
    -> st, (Some "Territory does not exist."), None
  | UnknownCont c
    -> st, (Some "Continent does not exist."), None
  | UnknownPlayer p
    -> st, (Some "Player does not exist."), None
  | NotOwner n
    -> st, (Some "You don't control this territory"), None
  | Failure s when s = "int_of_string" 
    -> st, (Some "Invalid integer"), None
  | _ -> st, msg, None

(** [print_message st msg search] prints the message for the current game
    state [st]. If [search] is successful, then prints the search message;
    otherwise, prints [msg]. *)
let print_message st msg (search : string * bool) =
  match msg, search with
  | Some m, _ -> print_endline m
  | None, (s,success) when String.length s > 0 -> 
    if success 
    then print_endline ("Search: " ^ s)
    else begin
      ANSITerminal.(print_string [] "Failing search: "; 
                    print_string [red] s);
      print_endline ""
    end
  | None, _ -> print_endline "..."

(** [game_loop_new st msg] continuously prompts the player for input
    and updates the game state according to the user input and the current 
    state [st]. Reprompts if invalid inputs are given and displays error
    message [msg].

    Helper for [risk f]. *)
let rec game_loop_new ?(search : string * bool = "",false) 
    (st : Interface.t) (msg : string option) : unit =

  draw_board st;
  win_yet (game_state st);
  (* print message *)
  print_message st msg search;
  (* drawing leaderboard *)
  if (Interface.leaderboard_on st) then draw_stats (st) else ();
  (* drawing help menu *)
  if (Interface.help_on st) then
    (begin match (leaderboard_on st), (turn (game_state st)) with
       | true, _ -> pick_help st "leaderboard"
       | false, Pick _ -> pick_help st "pick"
       | false, _ -> pick_help st "game"
     end;)
  else ();
  (* parsing inputs *)
  let st',msg',search' = parse_input st msg search
  in match search' with
  | Some s -> game_loop_new ~search:s st' msg'
  | None -> game_loop_new st' msg'

(** [insert_players pl msg t c] continuously prompts the user for input 
    involving the creation of players, saved in [pl]. Reprompts if invalid 
    inputs are given and displays error message [msg].Caps at however many 
    colors are provided in [c]. [t] is a flag indicating whether the add player 
    message is activated. *)
let rec insert_players 
    (pl:Player.t list) 
    (c:color list) 
    (t:bool) 
    (msg:string) : Player.t list = 
  ANSITerminal.set_cursor 0 (height ());
  ANSITerminal.erase Screen;
  print_endline "(a)dd a player";
  print_endline "(d)elete the last player added";
  print_endline "(s)tart the game with current players\n";
  print_players (List.rev pl);
  print_endline "";
  (* different inputs depending on whether you're adding a player or not *)
  if (t) then (print_endline (msg ^ "\n");
               begin match read_line (), c, pl with
                 | name, color::rest, _ 
                   -> if (String.trim name <> "") then
                     begin
                       insert_players ((Player.create name color) :: pl) 
                         rest false ("...")
                     end
                   else insert_players pl c t ("Can't have an empty name")
                 | _, _, _ -> insert_players pl c t msg
               end)
  else (print_endline (msg ^ "\n");
        begin
          match read_input (), c, pl with
          | "a", [], _ -> insert_players pl c t ("Can't add any more players!")
          | "a", color::rest, _ 
            -> insert_players pl c true "Who is this player?"
          | "d", c, [] -> insert_players pl c t ("No players to delete!")
          | "d", c, player::rest 
            -> insert_players rest ((player_color player)::c) t "..."
          | "s", _, [] -> insert_players pl c t ("Need at least one player!")
          | "s", _, _ -> pl 
          | "\004", _, _ | "\027", _, _ 
            -> print_endline("\nThanks for playing!\n"); exit 0
          | _, _, _ -> insert_players pl c t msg
        end)

(** [risk f] starts the game in [f]. 

    Requires: file [f] is a valid JSON respresentation of a Risk! game. *)
let risk f = 
  let board = Board.from_json (Yojson.Basic.from_file f) in
  let players = List.rev 
      (insert_players [] [Red;Blue;Green;Yellow;Magenta;Cyan] false "...") in
  try game_loop_new (Game_state.init board players |> Interface.init) None with 
  | End_of_file -> print_endline("\nThanks for playing!\n"); exit 0

(** Ascii art splash screen. *)
let title =
  "\r\n         _            _         _            _        "
  ^"\r\n        /\\ \\         /\\ \\      / /\\         /\\_\\      "
  ^"\r\n       /  \\ \\        \\ \\ \\    / /  \\       / / /  _   "
  ^"\r\n      / /\\ \\ \\       /\\ \\_\\  / / /\\ \\__   / / /  /\\_\\ "
  ^"\r\n     / / /\\ \\_\\     / /\\/_/ / / /\\ \\___\\ / / /__/ / / "
  ^"\r\n    / / /_/ / /    / / /    \\ \\ \\ \\/___// /\\_____/ /  "
  ^"\r\n   / / /__\\/ /    / / /      \\ \\ \\     / /\\_______/   "
  ^"\r\n  / / /_____/    / / /   _    \\ \\ \\   / / /\\ \\ \\      "
  ^"\r\n / / /\\ \\ \\  ___/ / /__ /_/\\__/ / /  / / /  \\ \\ \\     "
  ^"\r\n/ / /  \\ \\ \\/\\__\\/_/___\\\\ \\/___/ /  / / /    \\ \\ \\    "
  ^"\r\n\\/_/    \\_\\/\\/_________/ \\_____\\/   \\/_/      \\_\\_\\   "

(** [game ()] prompts for the game json file to load and then starts it. 
    Reprompts if the user gives an invalid file. Invalid file includes files not
    in the current directory, files without .json extension, or files that do 
    not exist. *)
let rec game () = 
  ANSITerminal.(print_string [red]
                  ("\n\nWelcome to..." ^ title ^ "\n\n"));
  print_endline "Please enter the map file you want to load:";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> try risk file_name with 
    | Sys_error _ -> print_endline("\nInvalid file"); game ()
    | Yojson.Json_error _ -> 
      print_endline("\nInvalid file, unable to parse JSON"); game ()

(* execute game *)
let () = game ()

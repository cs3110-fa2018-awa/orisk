open Game_state
open Command
open Board
open Board_state
open Display
open Player
open Interface

(** [helpmsg] is the string containing all possible commands, which is displayed 
    to the player when they enter the command [Help]. *)
let helpmsg = "[attack t1 t2 n] - attack t2 with n invading armies from t1\n
[reinforce t] - reinforce territory t with 1 army\n
[end] - end your turn\n
[help] - list possible commands\n 
[quit] - quit game" (* TODO: seems kind of weird to put help in help y'know *)

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

let next_valid_node st =
  let node = cursor_node st
  in let lst = turn_valid_nodes st
  in let rec helper = function
      | hd :: next :: tl when hd = node -> next
      | hd :: [] when hd = node -> List.hd lst
      | [] -> List.hd lst
      | hd :: tl -> helper tl
  in if List.length lst = 0 then None else Some (helper lst)

let read_input () =
  let buf = Bytes.create 8
  (* Inspired by https://stackoverflow.com/a/13410456 *)
  in let termio = Unix.tcgetattr Unix.stdin
  in let () = Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
         {termio with c_icanon = false; c_echo = false}
  in let len = input stdin buf 0 8
  in let () = Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio
  in Bytes.sub_string buf 0 len

let char_regexp = Str.regexp "[A-Za-z]"

let game_stage st = match st |> game_state |> turn with 
  | Null -> pick st,None
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
  | Fortify FromSelectF -> Some (cursor_node st) |> change_from_fortify_node st,None
  | Fortify (ToSelectF node) -> fortify_select st (Some node) (Some (cursor_node st)),None
  | Fortify (CountF (n1,n2)) -> failwith "):"

let game_nums st num = match st |> game_state |> turn with 
  | Reinforce ((PlaceR node),_) -> change_game_st st (reinforce (game_state st) (cursor_node st) num),None
  | Attack (OccupyA (n1,n2)) -> change_game_st st (occupy (game_state st) n1 n2 num),None
  | Fortify (CountF (n1,n2)) -> fortify (game_state st) n1 n2 num |> change_game_st st,None
  | _ -> failwith "why numbers"

(** [game_loop_new st msg] continuously prompts the player for commands
    and updates the game state according to the user input and the current 
    state [st]. Reprompts if invalid commands are given and displays error
    message [msg].

    Helper for [risk f]. *)
let rec game_loop_new ?(search : string * bool = "",false) 
    (st : Interface.t) (msg : string option) : unit =

  let perform_search str : unit =
    let found_node = node_search (st |> Interface.board) str in
    game_loop_new ~search:(str,found_node <> None)
      (set_cursor_node st found_node) msg in

  draw_board st;
  win_yet (game_state st);
  if (Interface.leaderboard_on st) then draw_stats (st) else ();
  begin match msg, search with
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
  end;
  try 
    begin
      match game_state st |> turn with 
      | Null | Reinforce (SelectR,_) | Attack (AttackSelectA | DefendSelectA _) | Fortify (FromSelectF | ToSelectF _) ->
        begin match read_input () with
          | "\027[A" -> game_loop_new (move_arrow st Up) msg
          | "\027[D" -> game_loop_new (move_arrow st Left) msg
          | "\027[B" -> game_loop_new (move_arrow st Down) msg
          | "\027[C" -> game_loop_new (move_arrow st Right) msg
          | " " | "\n" -> let st',msg' = game_stage st in game_loop_new st' msg'
          | "?" 
            -> game_loop_new (change_game_st st (game_state st |> end_turn_step)) msg
          (* leaderboard toggles *)
          | "=" -> game_loop_new (toggle_leaderboard st) msg
          | "'" -> game_loop_new (set_leaderboard_cat st CatPlayer) msg
          | "," -> game_loop_new (set_leaderboard_cat st CatArmy) msg
          | "." -> game_loop_new (set_leaderboard_cat st CatNode) msg
          | "/" -> game_loop_new (set_leaderboard_cat st CatCont) msg
          | "\t" -> game_loop_new (set_cursor_node st (next_valid_node st)) msg
          | "\\" -> game_loop_new (change_game_st st (game_state st |> back_turn)) msg
          | "\004" | "\027" -> print_endline("\nThanks for playing!\n"); exit 0
          | c when Str.string_match char_regexp c 0
            -> perform_search ((fst search) ^ c)
          | "\127" -> if String.length (fst search) <= 1
            then game_loop_new st msg
            else perform_search (String.sub (fst search) 0
                                   (String.length (fst search) - 1))
          | "`" -> if (game_state st |> turn) = Null
            then game_loop_new (game_state st |> assign_random_nodes
                                |> change_game_st st) msg
            else game_loop_new st msg
          | _ -> game_loop_new st msg
        end 
      | Reinforce (PlaceR _,_) | Attack (OccupyA _) | Fortify (CountF _) ->
        begin
          print_string "min [], max [] > ";
          let num = read_line () |> int_of_string in
          let st',msg' = game_nums st num in
          game_loop_new st' msg'
        end
    end
  with
  | NoPlayers
    -> game_loop_new st (Some "No players!")
  | NonadjacentNode (node_id1,node_id2)
    -> game_loop_new st (Some (node_id1 ^ " is not adjacent to " ^ node_id2 ^ "!"))
  | NonconnectedNode (node_id1,node_id2)
    -> game_loop_new st (Some (node_id1 ^ " is not connected to " ^ node_id2 ^ "!"))
  | SameNode node_id
    -> game_loop_new st (Some ("You can't perform this action on the same territory!"))
  | InvalidState (turn_state)
    -> game_loop_new st (Some "Wrong type of turn.")
  | InsufficientArmies (node_id,army)
    -> game_loop_new st (Some ("You only have " ^ (string_of_int army) ^
                               " armies to attack with! You can't attack from " ^
                               node_id ^ "!"))
  | FriendlyFire player
    -> game_loop_new st (Some "You can't attack yourself!")
  | UnknownNode n
    -> game_loop_new st (Some "Territory does not exist.")
  | UnknownCont c
    -> game_loop_new st (Some "Continent does not exist.")
  | UnknownPlayer p
    -> game_loop_new st (Some "Player does not exist.")
  | NotOwner n
    -> game_loop_new st (Some "You don't control this territory")
  | Failure s when s = "int_of_string" -> game_loop_new st (Some "Invalid integer")
  | _ -> game_loop_new st msg

(* [players] is a temporarily hard-coded set of players. Only used for the 
    demo as we will implement the ability to enter your own players in the
    future. *)
let players = [
  Player.create "player_a" Red; 
  Player.create "player_b" Green;
  Player.create "player_c" Blue
]

(** [risk f] starts the game in [f]. 

    Requires: file [f] is a valid JSON respresentation of a Risk! game. *)
let risk f = 
  let board = Board.from_json (Yojson.Basic.from_file f) in 
  try game_loop_new (Game_state.init board players |> Interface.init) None with 
  | End_of_file -> print_endline("\nThanks for playing!\n"); exit 0

(** [game ()] prompts for the game json file to load and then starts it. 
    Reprompts if the user gives an invalid file. Invalid file includes files not
    in the current directory, files without .json extension, or files that do 
    not exist. *)
let rec game () = 
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Risk!\n");
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

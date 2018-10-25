open Game_state
open Command
open Board
open Board_state
open Display
open Player

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

(** [game_loop st msg] continuously prompts the player for commands
    and updates the game state according to the user input and the current 
    state [st]. Reprompts if invalid commands are given and displays error
    message [msg].

    Helper for [risk f]. *)
let rec game_loop (st:Game_state.t) (msg : string option) : unit =
  draw_board st;
  win_yet st;
  print_endline "";
  print_endline (match msg with
      | Some m -> m
      | None -> "..."); 
  print_endline "\nEnter a command";
  print_string  "> ";
  try begin match Command.parse (read_line ()) with
    | exception (Command.Malformed) ->
      game_loop st (Some "Invalid command")
    | exception (Command.Empty) ->
      game_loop st (Some "Please enter a command!")
    | Quit -> print_endline("\nThanks for playing!\n"); exit 0
    | Help -> game_loop st (Some helpmsg)
    | ReinforceC (n) -> game_loop (reinforce st n) None
    | AttackC (a,d,i)
      -> let st', attack, defend = attack st a d i
      in game_loop st' (Some ("A: " ^ (string_of_dice attack) 
                              ^ " vs D: " ^ (string_of_dice defend)))
    | FortifyC (n1,n2) -> game_loop (fortify st n1 n2) None
    | EndTurn -> game_loop (end_attack st) None end with
  | NoPlayers
    -> game_loop st (Some "No players!")
  | NonadjacentNode (node_id1,node_id2)
    -> game_loop st (Some (node_id1 ^ " is not adjacent to " ^ node_id2 ^ "!"))
  | InvalidState (turn_state)
    -> game_loop st (Some "Wrong type of turn.")
  | InsufficientArmies (node_id,army)
    -> game_loop st (Some ("You only have " ^ (string_of_int army) ^
                           " armies to attack with! You can't attack from " ^
                           node_id ^ "!"))
  | FriendlyFire player
    -> game_loop st (Some "You can't attack yourself!")
  | UnknownNode n
    -> game_loop st (Some "Territory does not exist.")
  | UnknownCont c
    -> game_loop st (Some "Continent does not exist.")
  | UnknownPlayer p
    -> game_loop st (Some "Player does not exist.")
  | NotOwner n
    -> game_loop st (Some "You don't control this territory")

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
  try game_loop (assign_random_nodes (Game_state.init board players)) None with 
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

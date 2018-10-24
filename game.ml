open Game_state
open Command
open Board
open Board_state
open Display
open Player

let helpmsg = "[attack t1 t2 n] - attack t2 with n invading armies from t1\n
[reinforce t] - reinforce territory t with 1 army\n
[end] - end your turn\n
[help] - list possible commands\n
[quit] - quit game"

let string_of_dice dice =
  let rec internal acc = function
    | [] -> acc ^ "]"
    | hd :: ((nxt :: nxt2) as tl) -> internal (acc ^ (string_of_int (hd + 1)) ^ ", ") tl
    | hd :: tl -> internal (acc ^ (string_of_int (hd + 1))) tl
  in internal "[" dice

(* check if any player has won yet *)
let win_yet (st:Game_state.t) : unit =
  (* takes in player list*)
  let rec internal (acc: Player.t list) =
    match acc with
    | [] -> ()
    | p :: rest -> 
      (* check if a player owns all possible continents yet *)
      if ((p |> Board_state.player_conts (st |> Game_state.board_st) |> List.length) =
          (st |> Game_state.board_st |> Board_state.board |> Board.conts |> List.length))
      then (ANSITerminal.print_string [Foreground (player_color p)] ((player_name p)^" wins!\n"); 
            ignore (exit 0)) (* todo: say which player won *)
      else ()
  in internal (st |> Game_state.players)

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
      in game_loop st' (Some ("A: " ^ (string_of_dice attack) ^ " vs D: " ^ (string_of_dice defend)))
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
                           " armies to attack with! You can't attack from " ^ node_id ^ "!"))
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

let players = [
  Player.create "player_a" Red; 
  Player.create "player_b" Green;
  Player.create "player_c" Blue
]

let risk f = 
  let board = Board.from_json (Yojson.Basic.from_file f) in 
  try game_loop (assign_random_nodes (Game_state.init board players)) None with 
  | End_of_file -> print_endline("\nThanks for playing!\n"); exit 0

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

let () = game ()

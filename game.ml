open Game_state
open Command
open Board
open Board_state

let rec game_loop (st:Game_state.t) : unit = 
  print_endline "\nEnter a command";
  print_string  "> "; 
  try begin match Command.parse (read_line ()) with
    | exception (Command.Malformed) ->
      print_endline("\nInvalid command"); 
      game_loop st
    | exception (Command.Empty) ->
      print_endline("\nPlease enter a command!"); 
      game_loop st
    | Quit -> print_endline("\nThanks for playing!\n"); exit 0
    | ReinforceC (n) -> game_loop (reinforce st n)
    | AttackC (a,d,i) -> game_loop (attack st a d i)
    | EndTurn -> game_loop (end_attack st) end with
  | NoPlayers -> 
    print_endline "\nNo players!";
    game_loop st
  | NonadjacentNode (node_id1,node_id2) -> 
    print_endline ("\n" ^ node_id1 ^ " is not adjacent to " ^ node_id2 ^ "!");
    game_loop st
  | InvalidState (turn_state) -> 
    print_endline "\nWrong type of turn.";
    game_loop st
  | InsufficientArmies (node_id,army) -> 
    print_endline ("\nYou only have " ^ (string_of_int army) ^ "armies! You can't attack" ^ node_id ^ "!");
    game_loop st  
  | FriendlyFire player -> 
    print_endline "\nYou can't attack yourself!";
    game_loop st 
  | UnknownNode n -> 
    print_endline "\nTerritory does not exist.";
    game_loop st
  | UnknownCont c -> 
    print_endline "\nContinent does not exist.";
    game_loop st
  | UnknownPlayer p ->
    print_endline "\nPlayer does not exist.";
    game_loop st

let players = [
  Player.create "player_a" Red; 
  Player.create "player_b" Green;
  Player.create "player_c" Blue
]

let risk f = 
  let board = Board.from_json (Yojson.Basic.from_file f) in 
  try game_loop (Game_state.init board players) with 
  | End_of_file -> print_endline("\nThanks for playing!\n"); exit 0

let rec game () = 
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Risk!\n");
  print_endline "Please enter the name of the map file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> try risk file_name with 
    | Sys_error _ -> print_endline("\nInvalid file"); game ()
    | Yojson.Json_error _ -> 
      print_endline("\nInvalid file, unable to parse JSON"); game ()

let () = game ()
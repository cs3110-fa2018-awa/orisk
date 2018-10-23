open Game_state

let rec game_loop board (st:Game_state.t) =
  let process (result) (msg : string) = match result with
    | NoPlayers -> 
      print_endline("\nNo players!");
      game_loop boarSd st
    | NonadjacentNode(node_id1 * node_id2) -> 
      print_endline("\n" node_id1 ^ " is not adjacent to " node_id2) ^ "!";
      game_loop board st
    | InvalidState(turn_state) -> 
      print_endline("\nYou can't reinforce here!\n";
                    game_loop board st
                  | InsufficientArmies(node_id * army) -> 
                    print_endline("\nYou only have " ^ army "armies! You can't attack" ^ node_id ^ "!\n";
                                  game_loop board st  
                                | FriendlyFire of Player.t option -> 
                                    print_endline("\nYou can't attack yourself!\n";
                                                  game_loop board st in
  print_endline "\nEnter a command";
  print_string  "> "; 
  match Command.parse (read_line ()) with
  | exception (Command.Malformed) ->
    print_endline("\nInvalid command"); 
    game_loop board st
  | exception (Command.Empty) ->
    print_endline("\nPlease enter a command!"); 
    game_loop board st
  | Quit -> print_endline("\Thanks for playing!\n"); exit 0
      game_loop board st

let risk f = 
  let board = Board.from_json (Yojson.Basic.from_file f) in 
  try game_loop board (Game_state.init board) with 
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
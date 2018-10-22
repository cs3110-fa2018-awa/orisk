type object_phrase = string list

(** A command is either:
     - Attack of the object phrase to attack
     - Reinforce of the object phrase of where to reinforce
     - Score (of current player) 
     - Quit
*)
type command = 
  | Attack of object_phrase
  | Reinforce of object_phrase
  | Score
  | Quit

(** Empty is raised when the provided string to parse is either empty
    or consists entirely of spaces. *)
exception Empty

(** Malformed is raised when the command string cannot be parsed. *)
exception Malformed

(** [parse str] returns the command parsed from [str] - Attack, Reinforce,
    Score, Quit. *)
let parse str =
  let parse_args args = if List.length args > 0 then args else raise Malformed
  (* remove empty strings resulting from consecutive spaces *)
  and string_list = String.split_on_char ' ' str 
                    |> List.filter (fun s -> String.length s > 0) in 
  match string_list with 
  | ("attack" | "fight") :: args -> Attack(parse_args args)
  | ("reinforce" | "fortify" | "strengthen") :: args -> Reinforce(parse_args args)
  | "quit" :: _ -> Quit
  | "score" :: _ -> Score
  | [] -> raise Empty
  | _ -> raise Malformed
open Board

(** A command is either:
     - Attack of the object phrase to attack
     - Reinforce of the object phrase of where to reinforce
     - EndTurn
     - Help
     - Quit
*)
type command = 
  | AttackC of (node_id * node_id * army)
  | ReinforceC of node_id
  | EndTurn
  | Help
  | Quit

(** Empty is raised when the provided string to parse is either empty
    or consists entirely of spaces. *)
exception Empty

(** Malformed is raised when the command string cannot be parsed. *)
exception Malformed

(** [parse str] returns the command parsed from [str] - AttackC, ReinforceC, 
    Quit. *)
let parse str =
  let parse_attack = function 
    | attack :: defend :: invade :: [] -> begin 
        try attack,defend,int_of_string invade with 
        | Failure _ -> raise Malformed end
    | _ -> raise Malformed in
  let parse_reinforce = function
    | node :: [] -> node
    | _ -> raise Malformed in
  (* remove empty strings resulting from consecutive spaces *)
  let string_list = String.split_on_char ' ' str 
                    |> List.filter (fun s -> String.length s > 0) in 
  match string_list with 
  | "attack" :: args -> AttackC (parse_attack args)
  | "reinforce" :: args -> ReinforceC (parse_reinforce args)
  | "end" :: _ -> EndTurn
  | "help" :: _ -> Help
  | "quit" :: _ -> Quit
  | [] -> raise Empty
  | _ -> raise Malformed
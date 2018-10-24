open Board

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
type command = 
  | AttackC of (node_id * node_id * army)
  | ReinforceC of node_id
  | FortifyC of (node_id * node_id)
  | EndTurn
  | Help
  | Quit

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] returns the command parsed from [str] - AttackC, ReinforceC, 
    Quit. *)
val parse : string -> command

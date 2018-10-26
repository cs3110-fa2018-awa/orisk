(** Player command parsing.*)

open Board

(** The type [command] represents a player command that is decomposed
    into a verb and possibly a [node_id] or [army]. *)
type command = 
  | AttackC of (node_id * node_id * army)
  | ReinforceC of node_id
  | FortifyC of (node_id * node_id)
  | EndTurnStep
  | Help
  | Quit

(** [Empty] is raised when an empty command is parsed. *)
exception Empty

(** [Malformed] is raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] returns the command parsed from [str] - [AttackC], [ReinforceC], 
    [EndTurn], [Help], [Quit]. *)
val parse : string -> command

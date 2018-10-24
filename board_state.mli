open Board
open Player

(** The type of a board state. *)
type t

(** [init b players] is the default state from board [b]. *)
val init : Board.t -> Player.t list -> t

(** [board s] is the board used by state [s]. *)
val board : t -> Board.t

(** [node_owner state id] is [Some player] if node [id] is owned by 
    [player], or [None] if [id] is not owned by anyone. *)
val node_owner : t -> node_id -> Player.t option

(** [node_army state id] is the army stationed at node [id] in [state]. *)
val node_army : t -> node_id -> army

(** [cont_owner state id] is [Some player] if continent [id] 
    is owned by [player], or [None] if [id] is not owned by anyone. *)
val cont_owner : t -> cont_id -> Player.t option

(** [player_nodes state player] is a list of the nodes
    owned by [player] in [state]. *)
val player_nodes : t -> Player.t -> node_id list

(** [player_conts state player] is a list of the continents
    owned by [player] in [state]. *)
val player_conts : t -> Player.t -> cont_id list

(** [player_army state player] is the total number of armies owned
    by [player] in [state]. *)
val player_army : t -> Player.t -> army

(** [player_reinforcements state player] is the total number of
    reinforcements that [player] recieves given the current board
    configuration. *)
val player_reinforcements : t -> Player.t -> army

(** [set_army state node army] is the new state resulting from setting
    [node] to have [army] armies in [state]. *)
val set_army : t -> node_id -> army -> t

(** [place_army state node army] is the new state resulting from adding
    [army] armies to [node] in [state]. *)
val place_army : t -> node_id -> army -> t

(** [set_owner state node player] is the new state resulting from
    changing ownership of [node] to [player] in [state]. *)
val set_owner : t -> node_id -> Player.t option -> t

(** [dfs node visited] is a special implementation of a depth first search that
    will only go along monochromatic paths. 
    Returns a list of nodes visited. *)
val dfs : t -> node_id -> node_id list -> node_id list

exception UnknownPlayer of Player.t

(** Representation of board state including a board, and the state of the
    players, territories, and continents as the game is played. *)

open Board
open Player

(** The abstract type representing a board state. *)
type t

(** The type representing a player's board statistics. *)
type player_stats = 
  {player : Player.t; army_tot : int; node_tot : int; cont_tot : int}

(** [stats_category] is the category that the board leaderboard can be sorted 
    by. *)
type stats_category = CatPlayer | CatArmy | CatNode | CatCont

(** [stats_player ps] is the player in [ps]. *)
val stats_player : player_stats -> Player.t

(** [stats_army ps] is the total armies owned by a player in [ps]. *)
val stats_army : player_stats -> army

(** [stats_nodes ps] is the total territories owned by a player in [ps]. *)
val stats_nodes : player_stats -> int

(** [stats_conts ps] is the total continents owned by a player in [ps]. *)
val stats_conts : player_stats -> int

(** [init b players] is the default board state from board [b]. *)
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

(** [get_players state] is the list of all existing players in [state]. *)
val get_players: t -> Player.t list

(** [player_stats] is the board statistics of a player, used internally.
    It contains the total number of armies, territories, and continents
    that a player owns. *)
val player_stats_make : t -> Player.t -> player_stats

(** [sorted_player_stats state category] is the list of all player statistics,
    sorted in descending order based on [category] in [state]. *)
val sorted_player_stats : stats_category -> t -> player_stats list

(** [player_cont_bonus state player] is the the number of bonus reinforcements
    that [player] receives due to the continents that they control. *)
val player_cont_bonus : t -> Player.t -> army

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

(** [owners state] is the list of players that control at least one
    node in [state]. *)
val owners : t -> (Player.t option) list

val player_frontiers : t -> Player.t -> node_id list

(** [UnknownPlayer player] is raised if [player] is not in the [players] of 
    the current [state]. *)
exception UnknownPlayer of Player.t

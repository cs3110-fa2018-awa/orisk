(** Representation of game state including the state of the board
    and the current player's turn. *)

open Player
open Board_state
open Board

type reinforce_step = SelectR | PlaceR of node_id
type attack_step = AttackSelectA | DefendSelectA of node_id | OccupyA of (node_id * node_id)
type fortify_step = FromSelectF | ToSelectF of node_id | CountF of (node_id * node_id)

(** The type of a turn.*)
type turn_state =
  | Pick
  | Reinforce of (reinforce_step * army)
  | Attack of attack_step
  | Fortify of fortify_step

(** The type of a list of players. *)
type players = Player.t list

(** The abstract type representing a game state. *)
type t

(** [NoPlayers] is raised if a game state is initialized with no
    players. *)
exception NoPlayers

(** [NonadjacentNode (n1,n2)] is raised when a player attempts to 
    attack from node [n1] to node [n2] but [n1] and [n2] are not
    adjacent nodes. *)
exception NonadjacentNode of (node_id * node_id)

(** [NonconnectedNode (n1,n2)] is raised when a player attempts to 
    fortify from node [n1] to node [n2] but [n1] and [n2] are not
    connected by a path of nodes that the player owns. *)
exception NonconnectedNode of (node_id * node_id)

(** [SameNode n] is raised when a player attempts to perform an
    action meant for two different nodes on the same node. *)
exception SameNode of node_id

(** [InvalidState turn_st] is raised when a players inputs a 
    command that does not correspond to the current state of their
    turn [turn_st]. *)
exception InvalidState of turn_state

(** [InsufficientArmies (n,a)] is raised when a player attempts
    to invade from node [n] with more armies than [a] or an invalid 
    number of armies such as 0 or less. Also raised when the player
    attempts to move in less armies to the defending node than they 
    attack with, in the event that they win the battle. *)
exception InsufficientArmies of (node_id * army)

(** [FriendlyFire (Some p)] is raised when player [p] attempts to attack 
    a node they own. *)
exception FriendlyFire of Player.t option

(** [NotOwner node_id] is raised when a player attempts to reinforce or attack
    from a node [node_id] that they do not own.*)
exception NotOwner of node_id

(** [init b_st players] is the default game state from board state [b_st] and 
    list of players [players]. *)
val init : Board.t -> players -> t

(** [board_st st] is the board state of [st]. *)
val board_st : t -> Board_state.t

(** [players st] is the list [players] of [st]. *)
val players : t -> players

(** [current_player st] is the current [player] of [st]. *)
val current_player : t -> Player.t

(** [turn st] is the [turn_state] of [st]. *)
val turn : t -> turn_state

(** [turn_to_str st] is the string of the [turn_state] of [st]. *)
val turn_to_str : t -> string

(** [turn_to_attack st] is the game state [st] with the [turn_state] [Attack].*)
val turn_to_attack : t -> t

(** [change_board_st st board_st] is the game state [st] with board state 
    [board_st]. *)
val change_board_st : t -> Board_state.t -> t

(** [remaining_reinforcements st] is the number of armies the current [player] 
    of [st] has remaining. 

    Raises [InvalidState turn] when [turn] is not [Reinforce]. *)
val remaining_reinforcements : t -> army

(** [reinforce st n] is the game state resulting from the current [player] of 
    [st] adding one army to node [n]. 

    Raises [NotOwner n] if current [player] is not the owner of node [n] and
    [InvalidState turn] when [turn] is not [Reinforce]. *)
val reinforce : t -> node_id -> army -> t

(** [attack st a d invading_armies] is the game state [st] after node [a] 
    attacks node [d]. Each pair of attacking and defending armies constitutes 
    a battle in which the winner is determined according to rolling random die
    and the greater roll wins. If there is a tie, [d] wins. 
    If [a] wins the battle, [d] loses one army and vice versa. 

    If [d] reaches 0 armies, the current [player] of [st] moves 
    [invading_armies] amount of armies to node [d] and takes ownership of [d]. 

    [d] can only defend with [min 2 n1] where [n1] is the total number of armies
    on node [d]. [a] can only attack with [min 3 n2] where [n2] is the total
    number of armies on node [a]. 

    Raises: 
        - [InvalidState turn] when [turn] is not [Attack]
        - [NonadjacentNode (a,d)] if [a] and [d] are not adjacent
        - [NotOwner] if current [player] of [st] does not own [a]
        - [FriendlyFire (Some p)] if current player [p] of [st] owns both 
          [a] and [d]
        - [SameNode n] if [n] is both the attacking and defending node *)
val attack : t -> node_id -> node_id -> army -> t * int list * int list

(** [assign_random_nodes st] is the game state [st] after assigning 
    ownership of the nodes in [st] as equally as possible to each [player] in
    [st]. *)
val assign_random_nodes : t -> t

(** [end_turn_step st] is the game state [st] resulting from skipping the
    current turn step. If in reinforce, then moves to attack. If in attack,
    then moves to fortify. If in fortify, then advances to the next player's
    reinforce. *)
val end_turn_step : t -> t

val remaining_reinforcements : t -> army

(** [fortify st f t] sends one army from territory [f] to territory [t] if 
    they are connected by a path of territories that the current player owns.

    Raises:
        - [InvalidState turn] when [turn] is not [Fortify]
        - [NotOwner] if current [player] of [st] does not own [from_node]
        - [NotOwner] if current [player] of [st] does not own [to_node]
        - [SameNode n] if [n] is both the node fortifying from and to
        - [NonconnectedNode n1, n2] if [n1] and [n2] are not connected by a path
          of nodes owned by the current player
        - [InsufficientArmies n] if [n] does not have enough armies to fortify
          with *)
val fortify : t -> node_id -> node_id -> army -> t

val pick_nodes : t -> node_id -> t

(** [set_turn state turn] is [state] with its turn state changed to [turn]. *)
val set_turn : t -> turn_state -> t

val back_turn : t -> t

val occupy : t -> node_id -> node_id -> army -> t

val min_max_default : t -> army * army * army

(** Representation of game state including the state of the board
    and the current player's turn. *)

open Player
open Board_state
open Board

(** [reinforce_step] is the type of step within the reinforce stage:
    either [SelectR], in which the player selects a territory to
    reinforce, or [PlaceR node], in which the player places armies
    onto [node]. *)
type reinforce_step =
  | SelectR
  | PlaceR of node_id

(** [attack_step] is the type of step within the attack stage:
    either [AttackSelectA], in which the player selects the node
    to attack with, [DefendSelectA attacker], in which the player
    selects the node to attack to from [attacker], and [OccupyA
    (attacker, defender)], in which the player chooses how many
    armies to move from [attacker] to [defender] (after winning). *)
type attack_step =
  | AttackSelectA
  | DefendSelectA of node_id
  | OccupyA of (node_id * node_id)

(** [turn_state] is the type of step within the fortify stage:
    either [FromSelectF], in which the player selects the node
    to fortify from, [ToSelectF from], in which the player selects
    the node to fortify to from [from], and [CountF (from, to)],
    in which the player chooses how many troops to move from [from]
    to [to]. *)
type fortify_step =
  | FromSelectF
  | ToSelectF of node_id
  | CountF of (node_id * node_id)

(** The type of a turn. 
    - [Pick army], in which the players rotate
    through to select nodes at the beginning of the game until 
    [army] is exhausted
    - [Trade], in
    which the player can choose to trade in their stars for more
    reinforcements 
    - [Reinforce (reinforce_step, army)], in which the
    player reinforces [army] troops to nodes of their choosing,
    - [Attack (attack_step, bool)], in which the player attacks other players'
    nodes (and keeps track of whether the player has won a battle)
    - [Fortify fortify_step], in which the player fortifies
    troops from one node that they control to another that they control. *)
type turn_state =
  | Pick of army
  | Trade
  | Reinforce of (reinforce_step * army)
  | Attack of (attack_step * bool)
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

(** [InvalidState turn_st] is raised when a players attempts a 
    command that does not correspond to the current state of their
    turn [turn_st]. *)
exception InvalidState of turn_state

(** [InsufficientArmies (n,a)] is raised when a player attempts
    to invade from node [n] with more armies than [a] or an invalid 
    number of armies such as 0 or less. Also raised when the player
    attempts to move in less armies to the defending node than they 
    attack with, in the event that they win the battle. *)
exception InsufficientArmies of (node_id * army)

(** [InsufficientStars n] is raised when a player attempts to 
    trade in [n] stars when they don't own [n] stars. *)
exception InsufficientStars of int

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
    [invading_armies] armies (minus those lost in the battle) to node [d] and
    takes ownership of [d]. 

    [d] can only defend with [min 2 n1] where [n1] is the total number of armies
    on node [d]. [a] can only attack with [min 3 invading_armies].
    [invading_armies] must be less than or equal to [n2], where [n2] is the
    total number of armies on node [a].

    The resulting turn state is Attack OccupyA.

    Raises: 
        - [InvalidState turn] when [turn] is not [Attack]
        - [NonadjacentNode (a,d)] if [a] and [d] are not adjacent
        - [NotOwner] if current [player] of [st] does not own [a]
        - [FriendlyFire (Some p)] if current player [p] of [st] owns both 
          [a] and [d]
        - [SameNode n] if [n] is both the attacking and defending node
        - [InsufficientArmies (a, invading_armies)] if node [a] does not have
          enough armies to attack with [invading_armies]. *)
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

(** [fortify st f t army] sends [army] armies from territory [f] to territory
    [t] if they are connected by a path of territories that the current player
    owns.

    Raises:
        - [InvalidState turn] when [turn] is not [Fortify]
        - [NotOwner] if current [player] of [st] does not own [from_node]
        - [NotOwner] if current [player] of [st] does not own [to_node]
        - [SameNode n] if [n] is both the node fortifying from and to
        - [NonconnectedNode n1, n2] if [n1] and [n2] are not connected by a path
          of nodes owned by the current player
        - [InsufficientArmies (f, army)] if node [f] does not have enough
          armies to fortify with [army] *)
val fortify : t -> node_id -> node_id -> army -> t

(** [pick_nodes st node] is the result of the current player in [st] picking
    [node] during the [Pick] phase of the game; [node] becomes owned by the
    current player, if they are not already the owner, and has an army added.
    If the board is not full, players cannot pick a node that has already 
    been picked even if they are the owner. 

    If all armies have been used, then advances to the first turn, with game
    state [Trade].

    Raises [InvalidState st] if [turn] is not [Pick]. *)
val pick_nodes : t -> node_id -> t

(** [set_turn st turn] is [st] with its turn state changed to [turn]. *)
val set_turn : t -> turn_state -> t

(** [battle_won st] is whether the current player in attack state [st] has
    already won a battle in that turn. *)
val battle_won : t -> bool

(** [back_turn st] is [st] with the turn state reverted one step, but
    not leaving the current general turn state. This function behaves
    according to the following rules:

      - Pick -> Pick
      - Reinforce _ -> Reinforce SelectR
      - Attack _ -> Attack AttackSelectA
      - Fortify _ -> Fortify FromSelectF

    If the turn state was already in the result of applying this function,
    then the resulting turn state is unchanged. *)
val back_turn : t -> t

(** [occupy st a d occupying_armies] is the result of occupying node [d] from
    node [a] by moving [occupying_armies] from [a] to [d]. The resulting turn
    state is reset to [Attack AttackSelectA].

    Raises:
        - [InvalidState st] when [turn] is not [Attack OccupyA]
        - [InsufficientArmies (a, occupying_armies)] if node [a] does not have
          enough armies to fortify with [occupying_armies]. *)
val occupy : t -> node_id -> node_id -> army -> t

(** [min_max_default st] is a tuple of the minimum, maximum, and default
    number of troops that can be filled or the number of stars that
    can be traded for [st].

    This function is only defined for turn states that involve the selection
    of a number of troops or a number of stars - i.e. Reinforce PlacR, attack
    OccupyA, Fortify CountF, and Trade. For all other turn states, raises
    [InvalidState state]. *)
val min_max_default : t -> army * army * army

(** [turn_valid_nodes st] is the list of nodes that are able to be actioned
    upon during the current game state in interface [st]. *)
val turn_valid_nodes : t -> node_id list

(** [is_pick st] is true iff the turn state of [st] is [Pick]. *)
val is_pick : t -> bool 

(** [trade_stars st stars] is the result of the current player in [st] trading 
    in [stars] during the [Trade] phase of the game; the player will lose
    [stars] and will gain a number of armies to reinforce with. *)
val trade_stars : t -> int -> t

(** [game_state_of_json json] is the game state that [json] represents. *)
val game_state_of_json : Yojson.Basic.json -> t

(** [json_of_game_state gamestate] is the JSON assoc object
    representing [gamestate].*)
val json_of_game_state : t -> Yojson.Basic.json

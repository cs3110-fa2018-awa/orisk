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

(** The type of a turn. Either [Pick], in which the players rotate
    through to select nodes at the beginning of the game, [Reinforce
    (reinforce_step, army)], in which the player reinforces [army]
    troops to nodes of their choosing, [Attack attack_step], in which
    the player attacks other players' nodes, and [Fortify fortify_step],
    in which the player fortifies troops from one node that they
    control to another that they control. *)
type turn_state =
  | Pick
  | Reinforce of (reinforce_step * army)
  | Attack of attack_step
  | Fortify of fortify_step

(** The type of a list of players. *)
type players = Player.t list 

(** [Game_state.t] is the state of the game. The board state [Board_state.t], 
    list of players, current player, state of the current player's turn, and 
    remaining armies the current player has to reinforce with. *)
type t = {
  board_state : Board_state.t;
  players : players;
  current_player : Player.t;
  turn : turn_state;
}

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

(** [FriendlyFire (Some p)] is raised when player [p] attempts to attack 
    a node they own. *)
exception FriendlyFire of Player.t option

(** [NotOwner node_id] is raised when a player attempts to reinforce or attack
    from a node [node_id] that they do not own.*)
exception NotOwner of node_id

(** [init board players] is the initial [Game_state.t] with board state [board],
    and list of players [players]. The current player is the head of [players]
    and the turn state is [Reinforce]. The remaining reinforcements the current
    player has is [max(floor(n/3),3)] where [n] is the length of [players]. *)
let init board players =
  let board_st = Board_state.init board players in
  let curr_player = (match players with 
      | [] -> raise NoPlayers 
      | hd :: tl -> hd) in
  {
    board_state = board_st;
    players = players;
    current_player = curr_player;
    turn = Pick;
  }

(** [board_st st] is the board state of [st]. *)
let board_st st = st.board_state

(** [players st] is the list [players] of [st]. *)
let players st = st.players 

(** [current_player st] is the current [player] of [st]. *)
let current_player st = st.current_player 

(** [turn st] is the [turn_state] of [st]. *)
let turn st = st.turn

(** [turn_to_str st] is the string of the [turn_state] of [st]. *)
let turn_to_str st =
  match st.turn with
  | Pick -> "Picking territories"
  | Reinforce (_,remaining) -> "Reinforce " ^ (string_of_int remaining)
  | Attack AttackSelectA -> "Select attacker"
  | Attack DefendSelectA node -> "Attacking from " ^ node ^ ", select defender"
  | Attack OccupyA (node1,node2) -> "Move troops from " ^ node1 ^ " to " ^ node2
  | Fortify FromSelectF -> "Select territory to fortify from"
  | Fortify ToSelectF node -> "Fortifying from " ^ node ^ ", select destination"
  | Fortify CountF (node1,node2) -> "Move troops from " ^ node1 ^ " to " ^ node2 

(** [turn_to_attack st] is the game state [st] with the [turn_state] [Attack].*)
let turn_to_attack st = {st with turn = Attack AttackSelectA}

(** [set_turn st turn] is [st] with its turn state changed to [turn]. *)
let set_turn st turn = {st with turn = turn}

(** [change_board_st st board_st] is the game state [st] with board state 
    [board_st]. *)
let change_board_st st board_st = {st with board_state = board_st}

(*BISECT-IGNORE-BEGIN*) (*helpers not exposed and also play tested*)
(** [is_pick st] is true iff the turn state of [st] is [Pick].
    This is useful because the turn state may be parameterized, making
    it more difficult to determine the general term state without pattern
    matching. *)
let is_pick st = match st.turn with
  | Pick -> true
  | _ -> false

(** [is_reinforce st] is true iff the turn state of [st] is Reinforce.
    This is useful because the turn state may be parameterized, making
    it more difficult to determine the general term state without pattern
    matching. *)
let is_reinforce st = match st.turn with
  | Reinforce _ -> true
  | _ -> false

(** [is_attack st] is true iff the turn state of [st] is Attack.
    This is useful because the turn state may be parameterized, making
    it more difficult to determine the general term state without pattern
    matching. *)
let is_attack st = match st.turn with
  | Attack _ -> true
  | _ -> false

(** [is_fortify st] is true iff the turn state of [st] is Fortify.
    This is useful because the turn state may be parameterized, making
    it more difficult to determine the general term state without pattern
    matching. *)
let is_fortify st = match st.turn with
  | Fortify _ -> true
  | _ -> false
(*BISECT-IGNORE-END*)

(** [remaining_reinforcements st] is the number of armies the current [player] 
    of [st] has remaining. 

    Raises [InvalidState turn] when [turn] is not [Reinforce]. *)
let remaining_reinforcements st =
  match st.turn with 
  | Reinforce (_,remaining) -> remaining
  | _ -> raise (InvalidState st.turn)

(*BISECT-IGNORE-BEGIN*) (* extensive play testing*)
(** [reinforce st n] is the game state resulting from the current [player] of 
    [st] adding one army to node [n]. 

    Raises [NotOwner n] if current [player] is not the owner of node [n] and
    [InvalidState turn] when [turn] is not [Reinforce]. *)
let reinforce st n armies =
  if Some st.current_player <> (node_owner st.board_state n)
  then raise (NotOwner n) else (); 
  if not (is_reinforce st) then raise (InvalidState st.turn) else (); 
  if remaining_reinforcements st <= 0 then failwith "need more armies" else ();  
  if armies > remaining_reinforcements st || armies < 0
  then raise (InsufficientArmies (n,armies)) else (); (*better exception*)
  {st with board_state = place_army st.board_state n armies; 
           turn = if remaining_reinforcements st = armies
             then Attack AttackSelectA 
             else Reinforce (SelectR,remaining_reinforcements st - armies)}

(** [next_player curr_player lst] is the element in [lst] immediately after 
    [curr_player]. Returns the head of [lst] if [curr_player] is the last 
    element. *)
let next_player curr_player lst =
  let rec helper = function
    | hd :: next :: tl when hd = curr_player -> next
    | hd :: [] when hd = curr_player -> List.hd lst
    | [] -> failwith "current player isn't in players" 
    | hd :: tl -> helper tl
  in helper lst

(** [shuffle_lst lst] is list with the elements of [lst] randomly shuffled. *)
let shuffle_lst lst = QCheck.Gen.(generate1 (shuffle_l lst))

(** [assign_random_nodes st] is the game state [st] after assigning 
    ownership of the nodes in [st] as equally as possible to each [player] in
    [st]. *)
let assign_random_nodes (st : t) : t =
  let unselected node = (node_owner st.board_state node) = None in
  let st' =  List.fold_left
      (fun (st',player) (node : node_id) ->
         let next = next_player player st.players
         in ({st' with board_state
                       = place_army (set_owner st'.board_state node (Some next))
                           node 1} : t), next)
      (st,st.current_player) (board st.board_state |> nodes
                              |> List.filter unselected |> shuffle_lst) |> fst
  in let first_player = List.hd st.players in 
  {st' with turn = Reinforce 
                (SelectR,
                 Board_state.player_reinforcements st.board_state first_player);
            current_player = first_player}

(** [pick_nodes st node] is the result of the current player in [st] picking
    [node] during the [Pick] phase of the game; [node] becomes owned by the
    current player and has an army added.

    If all nodes have been picked, then advances to the first turn, with game
    state [Reinforce SelectR].

    Raises [InvalidState st] if [turn] is not [Pick]. *)
let pick_nodes st node =
  if not (is_pick st) then raise (InvalidState st.turn) else (); 
  if node_owner st.board_state node <> None then raise (NotOwner node) else ();
  let board_state = place_army 
      (set_owner st.board_state node (Some st.current_player)) node 1 in
  if List.mem None (owners board_state) then 
    {st with board_state = board_state; 
             current_player = next_player st.current_player st.players}
  else let first_player = List.hd st.players in 
    {st with board_state = board_state;
             turn = Reinforce 
                 (SelectR,player_reinforcements st.board_state first_player);
             current_player = first_player}

(** [setup_reinforce st] is the new state resulting from advancing [st] to the
    reinforce turn. [st.current_player] is advanced to the next player in the
    list and [st.turn] becomes [Reinforce (SelectR, n)], where [n] is the number
    of reinforcements that the next player should receive. *)
let setup_reinforce st =
  let next = next_player st.current_player st.players
  in {st with
      current_player = next;
      turn = Reinforce (SelectR,player_reinforcements st.board_state next)}

(** [end_turn_step st] is the game state [st] resulting from skipping the
    current turn step. If in reinforce, then moves to attack. If in attack,
    then moves to fortify. If in fortify, then advances to the next player's
    reinforce. *)
let end_turn_step st =
  match st.turn with
  | Pick -> st
  | Reinforce _ -> {st with turn = Attack AttackSelectA}
  | Attack _ -> {st with turn = Fortify FromSelectF}
  | Fortify _ -> setup_reinforce st

(** [back_turn st] is [st] with the turn state reverted one step, but
    not leaving the current general turn state. This function behaves
    according to the following rules:

      - Pick -> Pick
      - Reinforce _ -> Reinforce SelectR
      - Attack _ -> Attack AttackSelectA
      - Fortify _ -> Fortify FromSelectF

    If the turn state was already in the result of applying this function,
    then the resulting turn state is unchanged. *)
let back_turn st = 
  match st.turn with
  | Reinforce _ 
    -> {st with turn = Reinforce (SelectR,remaining_reinforcements st)}
  | Attack _ -> {st with turn = Attack AttackSelectA}
  | Fortify _ -> {st with turn = Fortify FromSelectF}
  | Pick -> st
(*BISECT-IGNORE-END*)

(** [rand_int_list acc num] is a list with [num] random ints in the range 0 to
    5, inclusive. *)
let rec rand_int_lst acc = function
  | 0 -> acc
  | n -> rand_int_lst ((Random.int 6) :: acc) (n - 1)

(** [battle attack defend (deatha,deathd)] is [(a,b)] where [a] is 
    the number of attacking armies lost and [b] is the number 
    of defending armies lost from comparing the respective elements of lists 
    [attack] and [defend] containing the result of random die rolls. If the
    roll in [attack] is greater than the corresponding roll in [defend],
    [deathd] increases by 1. And similarly, if the roll in [defend] is
    greater than the corresponding roll in [attack], [deatha] increases by 1.
    If the two rolls are the same, [deatha] increases by 1. *)
let rec battle attack defend (deatha,deathd) = 
  match attack,defend with 
  | ahd :: atl,dhd :: dtl ->
    if (ahd - dhd) > 0 (*BISECT-IGNORE-BEGIN*) (* play tested because random *)
    then battle atl dtl (deatha,deathd + 1)
    else battle atl dtl (deatha + 1,deathd)(*BISECT-IGNORE-END*)
  | _ -> deatha,deathd

(*BISECT-IGNORE-BEGIN*) (*extensive play test*)
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
let fortify st from_node to_node armies : t =
  if not (is_fortify st) then raise (InvalidState st.turn) else (); 
  if Some st.current_player <> (node_owner st.board_state from_node)
  then raise (NotOwner from_node) else ();
  if Some st.current_player <> (node_owner st.board_state to_node)
  then raise (NotOwner to_node) else ();
  if from_node = to_node
  then raise (SameNode to_node) else ();
  if not ((Board_state.dfs (st |> board_st) from_node []) |> List.mem to_node)
  then raise (NonconnectedNode (from_node,to_node)) else ();
  if (node_army st.board_state from_node) <= armies || armies < 0
  then raise (InsufficientArmies (from_node,armies)) else ();
  setup_reinforce 
    {st with board_state =  place_army 
                 (place_army st.board_state to_node armies) from_node (-armies)}
(*BISECT-IGNORE-END*)

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
        - [SameNode n] if [n] is both the attacking and defending node *)
let attack st a d invading_armies = 
  if not (is_attack st) then raise (InvalidState st.turn) else (); 
  if a = d then raise (SameNode d) else ();
  if not 
      (List.mem d (Board.node_borders (Board_state.board st.board_state) a)) 
  then raise (NonadjacentNode (a,d)) else (); 
  let attacker = Board_state.node_owner st.board_state a in
  if Some st.current_player <> (node_owner st.board_state a)
  then raise (NotOwner a) else ();
  if attacker = Board_state.node_owner st.board_state d 
  then raise (FriendlyFire attacker) else ();
  let total_attackers = (Board_state.node_army st.board_state a) - 1 in 
  let attack_armies = min total_attackers 3 in
  if attack_armies <= 0 || invading_armies > attack_armies 
  then raise (InsufficientArmies (a,attack_armies)) else ();
  let total_defenders = Board_state.node_army st.board_state d in
  let defend_armies = min total_defenders 2 in
  let attack_dice = 
    rand_int_lst [] attack_armies |> List.sort Pervasives.compare |> List.rev in
  let defend_dice = 
    rand_int_lst [] defend_armies |> List.sort Pervasives.compare |> List.rev in
  let attack_deaths,defend_deaths = battle attack_dice defend_dice (0,0) in 
  if defend_deaths = total_defenders 
  (* attacker won *)
  then {st with board_state = 
                  Board_state.set_army 
                    (Board_state.set_army 
                       (Board_state.set_owner st.board_state d attacker) d 
                       (invading_armies - attack_deaths)) a 
                    (total_attackers - invading_armies + 1);
                turn = Attack (OccupyA (a,d))}, 
       attack_dice, defend_dice
  (* attacker lost *)
  else {st with board_state = (*BISECT-IGNORE*) (* play tested *)
                  Board_state.set_army 
                    (Board_state.set_army st.board_state d 
                       (total_defenders - defend_deaths)) a 
                    (total_attackers - attack_deaths + 1);
                turn = Attack AttackSelectA}, 
       attack_dice, defend_dice

(*BISECT-IGNORE-BEGIN*) (*extensive play test*)
(** [occupy st a d occupying_armies] is the result of occupying node [d] from
    node [a] by moving [occupying_armies] from [a] to [d]. The resulting turn
    state is reset to [Attack AttackSelectA].

    Raises:
        - [InvalidState st] when [turn] is not [Attack OccupyA]
        - [InsufficientArmies (a, occupying_armies)] if node [a] does not have
          enough armies to fortify with [occupying_armies]. *)
let occupy st a d occupying_armies = 
  if st.turn <> Attack (OccupyA (a,d))
  then raise (InvalidState st.turn) else ();
  let total_attackers = (Board_state.node_army st.board_state a) - 1 in 
  if occupying_armies > total_attackers || occupying_armies < 0 
  (*TODO: better exception*)
  then raise (InsufficientArmies (a,occupying_armies)) else ();  
  {st with board_state = Board_state.place_army 
               (Board_state.place_army st.board_state a (~- occupying_armies)) d 
               occupying_armies;
           turn = Attack AttackSelectA}

(** [min_max_default st] is a tuple of the minimum, maximum, and default
    number of troops that can be filled for [st].

    This function is only defined for turn states that involve the selection
    of a number of troops - i.e. Reinforce PlacR, attack OccupyA, and Fortify
    CountF. For all other turn states, raises [InvalidState state]. *)
let min_max_default st : (army * army * army) = match st.turn with
  | Reinforce ((PlaceR node), remaining)
    -> (0, remaining, 1)
  | Attack (OccupyA (n1, n2))
    -> let max = (node_army st.board_state n1) - 1
    in (0, max, max)
  | Fortify (CountF (n1, n2))
    -> let max = (node_army st.board_state n1) - 1
    in (0, max, max)
  | _ -> raise (InvalidState st.turn)
(*BISECT-IGNORE-END*)

(* random seed *)
let () = Random.self_init ()

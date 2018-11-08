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

(** [InsufficientStars n] is raised when a player attempts to 
    trade in [n] stars when they don't own [n] stars. *)
exception InsufficientStars of int

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
    turn = let total_nodes = board |> nodes |> List.length in
      let total_players = players |> List.length in
      let armies = (total_nodes / total_players) + total_nodes in
      Pick (armies - (armies mod total_players));
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
  | Pick _ -> "Picking territories"
  | Trade -> "Choose how many stars to trade in for armies" 
  | Reinforce (_,remaining) -> "Reinforce " ^ (string_of_int remaining)
  | Attack (AttackSelectA, _) -> "Select attacker"
  | Attack (DefendSelectA node, _) 
    -> "Attacking from " ^ node_name (st.board_state |> board) node 
       ^ ", select defender"
  | Attack (OccupyA (node1,node2), _) 
    -> "Move troops from " ^ node_name (st.board_state |> board) node1 
       ^ " to " ^ node_name (st.board_state |> board) node2
  | Fortify FromSelectF -> "Select territory to fortify from"
  | Fortify ToSelectF node 
    -> "Fortifying from " ^ node_name (st.board_state |> board) node 
       ^ ", select destination"
  | Fortify CountF (node1,node2) 
    -> "Move troops from " ^ node_name (st.board_state |> board) node1 
       ^ " to " ^ node_name (st.board_state |> board) node2

(** [turn_to_attack st] is the game state [st] with the [turn_state] [Attack].*)
let turn_to_attack st = {st with turn = Attack (AttackSelectA, false)}

(** [set_turn st turn] is [st] with its turn state changed to [turn]. *)
let set_turn st turn = {st with turn = turn}

(** [change_board_st st board_st] is the game state [st] with board state 
    [board_st]. *)
let change_board_st st board_st = {st with board_state = board_st}

(** [battle_won st] is whether the current player in attack state [st] has
    already won a battle in that turn. *)
let battle_won st = 
  match st.turn with
  | Attack (_, won) -> won
  | _ -> failwith "Turn is not attack"

(*BISECT-IGNORE-BEGIN*) (*helpers not exposed and also play tested*)
(** [is_pick st] is true iff the turn state of [st] is [Pick].
    This is useful because the turn state may be parameterized, making
    it more difficult to determine the general term state without pattern
    matching. *)
let is_pick st = match st.turn with
  | Pick _ -> true
  | _ -> false

(** [is_trade st] is true iff the turn state of [st] is [Trade]. 
    This is useful because the turn state may be parameterized, making
    it more difficult to determine the general term state without pattern
    matching.*)
let is_trade st = match st.turn with
  | Trade -> true
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
  then raise (InsufficientArmies (n,armies)) else (); (*TODO: better exception*)
  {st with board_state = place_army st.board_state n armies; 
           turn = if remaining_reinforcements st = armies
             then Attack (AttackSelectA, false)
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
  {st' with turn = (let total_nodes = 
                      st.board_state |> board |> nodes |> List.length in
                    let total_players = st.players |> List.length in
                    Pick (total_nodes/total_players));
            current_player = first_player}

(** [pick_nodes st node] is the result of the current player in [st] picking
    [node] during the [Pick] phase of the game; [node] becomes owned by the
    current player, if they are not already the owner, and has an army added.
    If the board is not full, players cannot pick a node that has already 
    been picked even if they are the owner. 

    If all armies have been used, then advances to the first turn, with game
    state [Trade].

    Raises [InvalidState st] if [turn] is not [Pick]. *)
let pick_nodes st node =
  match st.turn with
  | Pick army ->
    let board_full = not (List.mem None (owners st.board_state)) in
    if node_owner st.board_state node <> None && not board_full
    then raise (NotOwner node) else ();
    let owner = node_owner st.board_state node in
    if owner <> Some (current_player st) && owner <> None 
    then raise (NotOwner node) else ();
    let board_state = place_army 
        (if board_full then st.board_state
         else set_owner st.board_state node (Some st.current_player)) node 1 in
    if army > 1
    then {st with board_state = board_state; 
                  current_player = next_player st.current_player st.players;
                  turn = Pick (army - 1)}
    else let first_player = List.hd st.players in 
      {st with board_state = board_state;
               turn = Trade;
               current_player = first_player}
  | _ -> raise (InvalidState st.turn)

(** [stars_to_armies] is the number of armies awarded based on the
    number of stars traded in. The max number of stars available to be
    traded in is 5. *)
let stars_to_armies = function 
  | 0 -> 0
  | 1 -> 1
  | 2 -> 2
  | 3 -> 4
  | 4 -> 7
  | 5 -> 10
  | _ -> failwith "shouldn't happen"

(** [trade_stars st stars] is the result of the current player in [st] trading 
    in [stars] during the [Trade] phase of the game; the player will lose
    [stars] and will gain a number of armies to reinforce with. *)
let trade_stars st (stars:int) =
  if not (is_trade st) then raise (InvalidState st.turn) else (); 
  if stars > player_stars st.board_state st.current_player
  then raise (InsufficientStars stars) else ();
  {st with 
   board_state = place_stars st.board_state st.current_player (-stars);
   turn = Reinforce 
       (SelectR, (player_reinforcements st.board_state st.current_player)
                 + stars_to_armies stars)}

(** [setup_trade state] is the new state resulting from advancing [state] to
    the trade turn. The current player is advanced to the next player. *)
let setup_trade st =
  let next = next_player st.current_player st.players
  in {st with current_player = next; turn = Trade}

(** [end_turn_step st] is the game state [st] resulting from skipping the
    current turn step. If in reinforce, then moves to attack. If in attack,
    then moves to fortify. If in fortify, then advances to the next player's
    reinforce. *)
let end_turn_step st =
  match st.turn with
  | Pick _ -> st
  | Trade -> {st with turn = Reinforce (SelectR,remaining_reinforcements st)}
  | Reinforce _ -> {st with turn = Attack (AttackSelectA, false)}
  | Attack _ -> {st with turn = Fortify FromSelectF}
  | Fortify _ -> setup_trade st

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
  | Attack _ -> {st with turn = Attack (AttackSelectA, battle_won st)}
  | Fortify _ -> {st with turn = Fortify FromSelectF}
  | Pick _ -> st
  | Trade -> st
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
    Once fortification is finished, the turn moves to [Trade].

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
  setup_trade
    {st with board_state =  place_army 
                 (place_army st.board_state to_node armies) from_node (-armies)}
(*BISECT-IGNORE-END*)

(** [place_stars_conditional st won] is the state with stars added to
    the current player in [st] only if the current player has not already 
    captured a territory during their overall attack turn, as flagged by 
    [won]. *)
let place_stars_conditional st won =
  if not won then 
    {st with board_state = place_stars 
                 st.board_state st.current_player (star_generator ())}
  else st

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
  then let add_stars_st = place_stars_conditional st (battle_won st) in
    let bs' = Board_state.set_army 
        (Board_state.set_army 
           (Board_state.set_owner add_stars_st.board_state d attacker) d 
           (invading_armies - attack_deaths)) a 
        (total_attackers - invading_armies + 1)
    in {add_stars_st
        with board_state = bs';
             players =
               if List.mem (node_owner add_stars_st.board_state d) (owners bs')
               then add_stars_st.players
               else List.filter (fun p ->
                   Some p <> node_owner add_stars_st.board_state d)
                   add_stars_st.players;
             turn = Attack ((OccupyA (a,d)), true)},
       attack_dice, defend_dice
    (* attacker lost *)
  else {st with board_state = (*BISECT-IGNORE*) (* play tested *)
                  Board_state.set_army 
                    (Board_state.set_army st.board_state d 
                       (total_defenders - defend_deaths)) a 
                    (total_attackers - attack_deaths + 1);
                turn = Attack (AttackSelectA, battle_won st)},
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
  if st.turn <> Attack ((OccupyA (a,d)), battle_won st)
  then raise (InvalidState st.turn) else ();
  let total_attackers = (Board_state.node_army st.board_state a) - 1 in 
  if occupying_armies > total_attackers || occupying_armies < 0 
  then raise (InsufficientArmies (a,occupying_armies)) else ();  
  {st with board_state = Board_state.place_army 
               (Board_state.place_army st.board_state a (~- occupying_armies)) d 
               occupying_armies;
           turn = Attack (AttackSelectA, true)}

(** [min_max_default st] is a tuple of the minimum, maximum, and default
    number of troops that can be filled or the number of stars that
    can be traded for [st].

    This function is only defined for turn states that involve the selection
    of a number of troops or a number of stars - i.e. Reinforce PlacR, attack
    OccupyA, Fortify CountF, and Trade. For all other turn states, raises
    [InvalidState state]. *)
let min_max_default st : (army * army * army) = match st.turn with
  | Reinforce ((PlaceR node), remaining)
    -> (0, remaining, 1)
  | Attack ((OccupyA (n1, n2)), _)
    -> let max = (node_army st.board_state n1) - 1
    in (0, max, max)
  | Fortify (CountF (n1, n2))
    -> let max = (node_army st.board_state n1) - 1
    in (0, max, max)
  | Trade -> let max = min 5 (player_stars st.board_state st.current_player)
    in (0, max, 0)
  | _ -> raise (InvalidState st.turn)
(*BISECT-IGNORE-END*)

(** [turn_valid_nodes st] is the list of nodes that are able to be actioned
    upon during the current game state in interface [st]. *)
let turn_valid_nodes gs =
  let bs = gs.board_state
  in let b = board bs
  in let is_owner = fun node -> node_owner bs node = Some (current_player gs)
  in let pred = match turn gs with
      | Pick army ->
        let is_picked = List.length
            (nodes_filter b (fun node -> node_owner bs node = None)) = 0
        in let player = Some (current_player gs)
        in fun node -> node_owner bs node = (if is_picked then player else None)
      | Reinforce (SelectR,_) -> is_owner
      | Reinforce (PlaceR _,_) -> failwith "shouldn't happen"
      | Attack (AttackSelectA,_)
        -> fun node -> node_owner bs node = Some (current_player gs)
                       && node_army bs node > 1
      | Attack ((DefendSelectA n),_)
        -> fun node -> node_owner bs node <> Some (current_player gs)
                       && List.mem node (node_borders b n)
      | Attack ((OccupyA _),_) -> failwith "shouldn't happen"
      | Fortify FromSelectF -> fun node -> is_owner node
                                           && node_army bs node > 1
      | Fortify (ToSelectF n)
        -> let reachable = dfs bs n []
        in fun node -> node <> n && List.mem node reachable
      | Fortify (CountF _) | Trade -> failwith "shouldn't happen"
  in nodes_filter b pred

(* random seed *)
let () = Random.self_init ()

open Yojson.Basic.Util

(* quick note: we don't care about the sub-turn state when saving/loading
   because these sub-states don't change the game at all. *)

let turn_of_json json =
  try begin
    let name = json |> member "name" |> to_string
    in let data = json |> member "data"
    in try
      begin
        match name with
        | "pick" -> Pick (data |> to_int)
        | "trade" -> Trade
        | "reinforce" -> Reinforce (SelectR, data |> to_int)
        | "attack" -> Attack (AttackSelectA, data |> to_bool)
        | "fortify" -> Fortify FromSelectF
        | _ -> failwith ("invalid turn name string: " ^ name)
      end with
    | ex -> failwith ("turn_of_json failed: " ^ (Printexc.to_string ex))
  end with
  | Yojson.Basic.Util.Type_error (msg, j) ->
    j |> Yojson.Basic.to_string |> print_endline;
    failwith ("failed to load game state turn: " ^ msg)

let json_of_turn turn =
  let name, data = match turn with
    | Pick army -> "pick", `Int army
    | Trade -> "trade", `Null
    | Reinforce (_, army) -> "reinforce", `Int army
    | Attack (_, starred) -> "attack", `Bool starred
    | Fortify _ -> "fortify", `Null
  in `Assoc [
    ("name", `String name);
    ("data", data);
  ]

let game_state_of_json json =
  try begin
    let players = json |> member "players" |> to_list |> List.map player_of_json
    in let current_player_id = json |> member "current_player" |> to_int
    in let current_player = List.find
           (fun player -> player_id player = current_player_id) players
    in {
      board_state = json |> member "board_state" |> board_state_of_json;
      players = players;
      current_player = current_player;
      turn = json |> member "turn" |> turn_of_json;
    }
  end with
  | Yojson.Basic.Util.Type_error (msg, j) ->
    j |> Yojson.Basic.to_string |> print_endline;
    failwith ("failed to load game state: " ^ msg)

let json_of_game_state gs =
  `Assoc [
    ("board_state", gs.board_state |> json_of_board_state);
    ("players", `List (List.map json_of_player gs.players));
    ("current_player", `Int (player_id gs.current_player));
    ("turn", json_of_turn gs.turn);
  ]

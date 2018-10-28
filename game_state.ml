open Player
open Board_state
open Board

type reinforce_step = SelectR | PlaceR
type attack_step = AttackSelectA | DefendSelectA | OccupyA
type fortify_step = FromSelectF | ToSelectF | CountF

(** The type of a turn.*)
type turn_state =
  | Null
  | Reinforce of reinforce_step
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
  remaining_reinforcements : army;
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
    turn = Null;
    remaining_reinforcements = player_reinforcements board_st curr_player;
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
  | Null -> "Picking territories"
  | Reinforce _ -> "Reinforce"
  | Attack AttackSelectA -> "Attack Select"
  | Attack DefendSelectA -> "Defend"
  | Attack OccupyA -> "Occupy"
  | Fortify FromSelectF -> "Fortify Select"
  | Fortify ToSelectF -> "Fortify To"
  | Fortify CountF -> "Fortify Count"

(** [turn_to_attack st] is the game state [st] with the [turn_state] [Attack].*)
let turn_to_attack st = {st with turn = Attack AttackSelectA}

let set_turn st turn = {st with turn = turn}

(** [change_board_st st board_st] is the game state [st] with board state 
    [board_st]. *)
let change_board_st st board_st = {st with board_state = board_st}

let is_null st = match st.turn with
  | Null -> true
  | _ -> false

let is_reinforce st = match st.turn with
  | Reinforce _ -> true
  | _ -> false

let is_attack st = match st.turn with
  | Attack _ -> true
  | _ -> false

let is_fortify st = match st.turn with
  | Fortify _ -> true
  | _ -> false

(** [remaining_reinforcements st] is the number of armies the current [player] 
    of [st] has remaining. 

    Raises [InvalidState turn] when [turn] is not [Reinforce]. *)
let remaining_reinforcements st =
  let () = if not (is_reinforce st) then raise (InvalidState st.turn) else () in
  st.remaining_reinforcements

(** [reinforce st n] is the game state resulting from the current [player] of 
    [st] adding one army to node [n]. 

    Raises [NotOwner n] if current [player] is not the owner of node [n] and
    [InvalidState turn] when [turn] is not [Reinforce]. *)
let reinforce st n armies =
  let () = if Some st.current_player <> (node_owner st.board_state n)
    then raise (NotOwner n) else () in
  let () = if not (is_reinforce st) then raise (InvalidState st.turn) else () in
  let () = if st.remaining_reinforcements <= 0 then failwith "need more armies"
    else () in 
  let () = if armies > st.remaining_reinforcements || armies <= 0
    then raise (InsufficientArmies (n,armies)) else () in (*better exception*)
  {st with board_state = place_army st.board_state n armies; 
           remaining_reinforcements = st.remaining_reinforcements - armies;
           turn = if st.remaining_reinforcements = armies
             then Attack AttackSelectA else Reinforce SelectR}

(*BISECT-IGNORE-BEGIN*) (* helper not exposed in mli, also play tested both*)
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

let shuffle_lst lst = QCheck.Gen.(generate1 (shuffle_l lst))

(** [assign_random_nodes st] is the game state [st] after assigning 
    ownership of the nodes in [st] as equally as possible to each [player] in
    [st]. *)
let assign_random_nodes (st : t) : t =
  let unselected node = (node_owner st.board_state node) = None in
  {begin
    List.fold_left
      (fun (st',player) (node : node_id) : (t * Player.t) ->
         let next = next_player player st.players
         in ({st' with board_state
                       = place_army (set_owner st'.board_state node (Some next))
                           node 1} : t), next)
      (st,st.current_player) (board st.board_state |> nodes
                              |> List.filter unselected |> shuffle_lst) |> fst
  end with turn = Reinforce SelectR}
(*BISECT-IGNORE-END*)

let pick_nodes st node =
  let () = if not (is_null st) then raise (InvalidState st.turn) else () in
  let () = if node_owner st.board_state node <> None then raise (NotOwner node) else () in (*better exception*)
  if List.mem None (owners st.board_state) then 
    {st with board_state = place_army (set_owner st.board_state node (Some st.current_player))
                 node 1; current_player = next_player st.current_player st.players}
  else {st with current_player = List.hd st.players;
                turn = Reinforce SelectR}

let init_reinforce st = {st with current_player = List.hd st.players; turn = Reinforce SelectR}

let setup_reinforce st =
  let next = next_player st.current_player st.players
  in {st with
      current_player = next;
      remaining_reinforcements = player_reinforcements st.board_state next;
      turn = Reinforce SelectR}

(*BISECT-IGNORE-BEGIN*) (* play tested *)
(** [end_turn_step st] is the game state [st] resulting from skipping the
    current turn step. If in reinforce, then moves to attack. If in attack,
    then moves to fortify. If in fortify, then advances to the next player's
    reinforce. *)
let end_turn_step st =
  match st.turn with
  | Null -> st
  | Reinforce _ -> {st with turn = Attack AttackSelectA}
  | Attack _ -> {st with turn = Fortify FromSelectF}
  | Fortify _ -> setup_reinforce st
(*BISECT-IGNORE-END*)

let back_turn st = 
  match st.turn with
  | Reinforce _ -> {st with turn = Reinforce SelectR}
  | Attack _ -> {st with turn = Attack AttackSelectA}
  | Fortify _ -> {st with turn = Fortify FromSelectF}
  | Null -> st

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

(** [fortify st f t] sends one army from territory [f] to territory [t] if 
    they are connected by a path of territories that the current player owns. *)
let fortify st (from_node : Board.node_id) (to_node : Board.node_id) : t =
  let () = if not (is_fortify st) then raise (InvalidState st.turn) else () 
  in let () = if Some st.current_player <> (node_owner st.board_state from_node)
       then raise (NotOwner from_node) else ()
  in let () = if Some st.current_player <> (node_owner st.board_state to_node)
       then raise (NotOwner to_node) else ()
  in let () = if from_node = to_node
       then raise (SameNode to_node) else ()
  in let () = if (node_army st.board_state from_node) <= 1
       then raise (InsufficientArmies (from_node,1)) else ()
  in let () = if not ((Board_state.dfs (st |> board_st) from_node []) |> List.mem to_node)
       then raise (NonconnectedNode (from_node,to_node)) else ()
  in setup_reinforce
    {st with board_state = place_army (place_army st.board_state to_node 1) from_node (-1)}

(** [attack st a d invading_armies] is the game state [st] after node [a] 
    attacks node [d]. Each pair of attacking and defending armies constitutes 
    a battle in which the winner is determined according to rolling random die
    and following the rules specified in [battle]. If a battle is successful,
    i.e. in favor of [a], then [d] loses one army. If a battle is not 
    successful, [a] loses one army. 

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
          [a] and [d] *)
let attack st a d invading_armies = 
  let () = if not (is_attack st) then raise (InvalidState st.turn) else () in
  let () = if a = d then raise (SameNode d) else () in
  let () = if not 
      (List.mem d (Board.node_borders (Board_state.board st.board_state) a)) 
    then raise (NonadjacentNode (a,d)) else () in 
  let attacker = Board_state.node_owner st.board_state a in
  let () = if Some st.current_player <> (node_owner st.board_state a)
    then raise (NotOwner a) else () in
  let () = if attacker = Board_state.node_owner st.board_state d 
    then raise (FriendlyFire attacker) else () in
  let total_attackers = (Board_state.node_army st.board_state a) - 1 in 
  let attack_armies = min total_attackers 3 in
  let () = if attack_armies <= 0 || invading_armies < attack_armies 
              || invading_armies > total_attackers
    then raise (InsufficientArmies (a,attack_armies)) else () in
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
                turn = Attack AttackSelectA}, 
       attack_dice, defend_dice
  (* attacker lost *)
  else {st with board_state = (*BISECT-IGNORE*) (* play tested *)
                  Board_state.set_army 
                    (Board_state.set_army st.board_state d 
                       (total_defenders - defend_deaths)) a 
                    (total_attackers - attack_deaths + 1);
                turn = Attack AttackSelectA}, 
       attack_dice, defend_dice

(* random seed *)
let () = Random.self_init ()

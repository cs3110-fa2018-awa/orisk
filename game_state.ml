open Player
open Board_state
open Board

type turn_state = Reinforce | Attack | Fortify

type players = Player.t list 

type t = {
  board_state : Board_state.t;
  players : players;
  current_player : Player.t;
  turn : turn_state;
  remaining_reinforcements : army;
}

exception NoPlayers
exception NonadjacentNode of (node_id * node_id)
exception InvalidState of turn_state
exception InsufficientArmies of (node_id * army)
exception FriendlyFire of Player.t option
exception NotOwner of node_id

let init board players =
  let board_st = Board_state.init board players in
  let curr_player = (match players with 
      | [] -> raise NoPlayers 
      | hd :: tl -> hd) in
  {
    board_state = board_st;
    players = players;
    current_player = curr_player;
    turn = Reinforce;
    remaining_reinforcements = player_reinforcements board_st curr_player;
  }

let board_st {board_state} = board_state

let players {players} = players 

let current_player {current_player} = current_player 

let turn {turn} = turn

let turn_to_str {turn} =
  match turn with
  | Reinforce -> "Reinforce"
  | Attack -> "Attack"
  | Fortify -> "Fortify"

let turn_to_attack st = {st with turn = Attack}

let change_board_st st board_st = {st with board_state = board_st}

let remaining_reinforcements st =
  let () = if st.turn <> Reinforce then raise (InvalidState st.turn) else () in
  st.remaining_reinforcements

let reinforce st n =
  let () = if Some st.current_player <> (node_owner st.board_state n)
    then raise (NotOwner n) else () in
  let () = if st.turn <> Reinforce then raise (InvalidState st.turn) else () in
  let () = if st.remaining_reinforcements <= 0 then failwith "need more armies"
    else () in 
  {st with board_state = place_army st.board_state n 1; 
           remaining_reinforcements = st.remaining_reinforcements - 1;
           turn = if st.remaining_reinforcements = 1 then Attack else Reinforce}

let next_player curr_player lst =
  let rec helper = function
    | hd :: next :: tl when hd = curr_player -> next
    | hd :: [] when hd = curr_player -> List.hd lst
    | [] -> failwith "current player isn't in players" 
    | hd :: tl -> helper tl
  in helper lst

let assign_random_nodes (st : t) : t =
  (* TODO not actually random right now *)
  fold_nodes (board st.board_state)
    (fun (node : node_id) ((st',player) : (t * Player.t)) ->
       let next = next_player player st.players
       in ({st' with board_state
                     = place_army (set_owner st'.board_state node (Some next))
                         node 1} : t), next)
    (st,st.current_player) |> fst

let end_attack st = {st with turn = Fortify}

let rec rand_int_lst acc = function
  | 0 -> acc
  | n -> rand_int_lst ((Random.int 6) :: acc) (n - 1)

let rec battle attack defend (deatha,deathd) = 
  match attack,defend with 
  | ahd :: atl,dhd :: dtl ->
    if (ahd - dhd) > 0 
    then battle atl dtl (deatha,deathd + 1) 
    else battle atl dtl (deatha + 1,deathd)
  | _ -> deatha,deathd

(** [fortify st f t] sends one army from territory [f] to territory [t] if 
   they are connected by a path of territories that the current player owns. *)
let fortify st (from_node : Board.node_id) (to_node : Board.node_id) : t =
  let () = if Some st.current_player <> (node_owner st.board_state from_node)
    then raise (NotOwner from_node) else ()
  in let () = if from_node = to_node
       then raise (NonadjacentNode (from_node,to_node)) else () (* TODO better exception *)
  in let () = if (node_army st.board_state from_node) <= 1
       then raise (InsufficientArmies (from_node,1)) else ()
  in let () = if not ((Board_state.dfs (st |> board_st) from_node []) |> List.mem to_node)
       then raise (NonadjacentNode (from_node,to_node)) else () (* TODO better exception *)
  in let next = next_player st.current_player st.players
  in {st with
      board_state = place_army (place_army st.board_state to_node 1) from_node (-1);
      current_player = next;
      remaining_reinforcements = player_reinforcements st.board_state next;
      turn = Reinforce}

(* one atack *)
let attack st a d invading_armies = 
  let () = if st.turn <> Attack then raise (InvalidState st.turn) else () in
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
  let () = if attack_armies <= 0 || invading_armies < attack_armies || invading_armies > total_attackers
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
                    (total_attackers - invading_armies + 1)}, attack_dice, defend_dice
  (* attacker lost *)
  else {st with board_state = 
                  Board_state.set_army 
                    (Board_state.set_army st.board_state d 
                       (total_defenders - defend_deaths)) a 
                    (total_attackers - attack_deaths + 1)}, attack_dice, defend_dice

(* random seed *)
let () = Random.self_init ()

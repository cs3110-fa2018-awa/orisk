open Player
open Board_state
open Board

type turn_state = Reinforce | Attack 

type players = Player.t list 

type t = {
  board_state : Board_state.t;
  players : players;
  current_player : Player.t;
  turn : turn_state
}

exception NoPlayers
exception NonadjacentNode of (node_id * node_id)
exception InvalidState of turn_state
exception InsufficientArmies of (node_id * army)
exception FriendlyFire of Player.t option

let init board players = {
  board_state = Board_state.init board players;
  players = players;
  current_player = (match players with 
      | [] -> raise NoPlayers 
      | hd :: tl -> hd);
  turn = Reinforce
}

let board_st {board_state} = board_state

let turn {turn} = turn

let reinforce st n =
  let () = if st.turn <> Reinforce then raise (InvalidState st.turn) else () in
  {st with board_state = Board_state.place_army st.board_state n 1}

let rec rand_int_lst acc = function
  | 0 -> acc
  | n -> rand_int_lst ((Random.int 6) :: acc) (n - 1)

let rec battle attack defend (deatha,deathd) = 
  match attack,defend with 
  | ahd :: atl,dhd :: dtl -> if (ahd - dhd) > 0 
    then battle atl dtl (deatha,deathd + 1) 
    else battle atl dtl (deatha + 1,deathd)
  | _ -> deatha,deathd

(* one atack *)
let attack st a d invading_armies = 
  let () = if st.turn <> Attack then raise (InvalidState st.turn) else () in
  let () = if not 
      (List.mem d (Board.node_borders (Board_state.board st.board_state) a)) 
    then raise (NonadjacentNode (a,d)) else () in 
  let attacker = Board_state.node_owner st.board_state a in
  let () = if attacker = Board_state.node_owner st.board_state d 
    then raise (FriendlyFire attacker) else () in
  let total_attackers = Board_state.node_army st.board_state a in 
  let attack_armies = min (total_attackers - 1) 3 in
  let () = if attack_armies <= 0 || invading_armies < attack_armies 
    then raise (InsufficientArmies (a,attack_armies)) else () in
  let total_defenders = Board_state.node_army st.board_state d in
  let defend_armies = min total_defenders 2 in
  let attack_dice = 
    rand_int_lst [] attack_armies |> List.sort Pervasives.compare in
  let defend_dice = 
    rand_int_lst [] defend_armies |> List.sort Pervasives.compare in
  let attack_deaths,defend_deaths = battle attack_dice defend_dice (0,0) in 
  if defend_deaths = total_defenders 
  then {st with board_state = 
                  Board_state.set_army 
                    (Board_state.set_army 
                       (Board_state.set_owner st.board_state d attacker) d 
                       (invading_armies - attack_deaths)) a 
                    (total_attackers - invading_armies)} 
  else {st with board_state = 
                  Board_state.set_army 
                    (Board_state.set_army st.board_state d 
                       (total_defenders - defend_deaths)) a 
                    (total_attackers - attack_deaths)}
open Move
open Game_state
open Board_state

type move_edge = {
  move : move; 
  outcomes : move_tree list
}
and move_tree = {
  game_state : Game_state.t; 
  probability : float; 
  moves : move_edge list
}

let attack_tree gs attacker defender armies = 
  let total_attackers = (Board_state.node_army (board_st gs) attacker) - 1 in 
  let attack_armies = min total_attackers 3 in
  let total_defenders = Board_state.node_army (board_st gs) defender in
  let defend_armies = min total_defenders 2 in
  let attack_outcome gs a d = 
    if d = total_defenders then begin 
      let board_state = Board_state.set_army 
          (Board_state.set_army 
             (Board_state.set_owner (board_st gs) defender (node_owner (board_st gs) attacker)) defender
             (armies - a)) attacker
          (total_attackers - armies + 1) in 
      let gs' = change_board_st gs board_state in 
      set_turn gs' (Attack (OccupyA (attacker,defender)))
    end
    else begin
      let board_state = Board_state.set_army 
          (Board_state.set_army (board_st gs) defender 
             (total_defenders - d)) attacker 
          (total_attackers - a + 1) in
      let gs' = change_board_st gs board_state in
      set_turn gs' (Attack AttackSelectA)
    end in
  let outcomes lst =
    List.fold_left 
      (fun acc (a,d,p) -> 
         {game_state = attack_outcome gs a d;
          probability = p; 
          moves = []} :: acc) [] lst in
  match attack_armies,defend_armies with
  | 1,1 -> outcomes [(1,0,0.5833);(0,1,0.4267)]
  | 1,2 -> outcomes [(1,0,0.7454);(0,1,0.2546)]
  | 2,1 -> outcomes [(1,0,0.4213);(0,1,0.5787)]
  | 2,2 -> outcomes [(2,0,0.4483);(1,1,0.3241);(0,2,0.2276)]
  | 3,1 -> outcomes [(1,0,0.6597);(0,1,0.3403)]
  | 3,2 -> outcomes [(2,0,0.2926);(1,1,0.3358);(0,2,0.3717)]
  | _ -> failwith "shouldn't happen"

let move_probabilities gs move = 
  match (turn gs), move with
  | Pick, PickM node
    -> [{game_state = pick_nodes gs node; probability = 1.; moves = []}]
  | Reinforce (SelectR, _), ReinforceM (node, army)
    -> [{game_state = reinforce gs node army; probability = 1.; moves = []}]
  | Attack AttackSelectA, AttackM (attacker, defender, army)
    -> attack_tree gs attacker defender army
  | Attack (OccupyA (attacker, defender)), OccupyM army
    -> [{game_state = occupy gs attacker defender army; probability = 1.; moves = []}]
  | Fortify FromSelectF, FortifyM (from_node, to_node, army)
    -> [{game_state = fortify gs from_node to_node army; probability = 1.; moves = []}]
  | (Attack AttackSelectA | Fortify FromSelectF), FinishM
    -> [{game_state = end_turn_step gs; probability = 1.; moves = []}]
  | _, FinishM -> [{game_state = gs; probability = 1.; moves = []}]
  | _ -> failwith ("invalid state/move combination: " ^ (string_of_move move))


open Move
open Game_state
open Board_state
open Heuristic

type move_edge = {
  move : move; 
  outcomes : move_tree list;
  heuristic : float;
}
and move_tree = {
  game_state : Game_state.t; 
  probability : float; 
  moves : move_edge list;
}

let rec move_edge_heuristic gs move_trees personality = 
  let internal {game_state;moves} = 
    List.map (fun edge -> move_edge_heuristic game_state edge.outcomes personality) moves |>
    List.sort Pervasives.compare |> List.rev |> List.hd in
  let heur tree = match tree.moves with
    | [] -> heuristic tree.game_state personality (current_player gs)
    | _ -> internal tree in
  List.fold_left 
    (fun acc ({probability} as tree) ->
       acc+.(probability*.(heur tree))) 0. move_trees

let attack_tree gs attacker defender armies = 
  let total_attackers = (Board_state.node_army (board_st gs) attacker) - 1 in 
  let attack_armies = min total_attackers 3 in
  let total_defenders = Board_state.node_army (board_st gs) defender in
  let defend_armies = min total_defenders 2 in
  let attack_outcome gs a d = 
    if d = total_defenders then begin 
      let board_state = Board_state.set_army 
          (Board_state.set_army 
             (Board_state.set_owner (board_st gs) defender 
                (node_owner (board_st gs) attacker)) defender 
             (armies - a)) attacker (total_attackers - armies + 1) in 
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
  (* Probabilities from https://web.stanford.edu/~guertin/risk.notes.html *)
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
  | Reinforce (SelectR, _), ReinforceM list
    -> [{game_state = List.fold_left
             (fun acc (node, army) -> reinforce acc node army) gs list;
         probability = 1.; moves = []}]
  | Attack AttackSelectA, AttackM (attacker, defender, army)
    -> attack_tree gs attacker defender army
  | Attack (OccupyA (attacker, defender)), OccupyM army
    -> [{game_state = occupy gs attacker defender army; probability = 1.; 
         moves = []}]
  | Fortify FromSelectF, FortifyM (from_node, to_node, army)
    -> [{game_state = fortify gs from_node to_node army; probability = 1.; 
         moves = []}]
  | (Attack AttackSelectA | Fortify FromSelectF), FinishM
    -> [{game_state = end_turn_step gs; probability = 1.; moves = []}]
  | _, FinishM -> [{game_state = gs; probability = 1.; moves = []}]
  | _ -> failwith ("invalid state/move combination: " ^ (string_of_move move))

let heuristic_compare e1 e2 =
  Pervasives.compare e1.heuristic e2.heuristic

let first_n list n =
  let rec internal acc = function
    | _,0 | [],_ -> acc
    | hd :: tl, n' -> internal (hd :: acc) (tl, n')
  in internal [] (list, n) |> List.rev

let rec move_edge gs personality depth move = 
  (*print_endline ("move edge "^(string_of_int depth));*)
  let fill_moves move_tree =
    let all_moves = valid_moves move_tree.game_state
    in let pairs = List.map (fun move -> (move, heuristic move_tree.game_state personality (current_player gs))) all_moves
    in let sorted_moves = List.sort (fun (_, h1) (_, h2) -> Pervasives.compare h2 h1) pairs
                          |> List.map fst
    in let best_moves = first_n sorted_moves (2 * depth)
    in {move_tree with moves = List.map (move_edge move_tree.game_state personality 
                                        (depth-1)) best_moves} in 
  let move_trees = match depth,(turn gs) with
    | 0,_ | _,Fortify _ -> move_probabilities gs move
    | _ -> List.map fill_moves (move_probabilities gs move)
  in {move = move; outcomes = move_trees; 
      heuristic = move_edge_heuristic gs move_trees personality}

let move_tree gs probability personality depth = 
  let edges = List.map (move_edge gs personality depth) (valid_moves gs) in 
  {game_state = gs; probability = probability; moves = edges}

let best_move gs depth =
  let tree = move_tree gs 1. Personality.default depth in
  print_endline ("current gs heuristic "^(string_of_float (Heuristic.heuristic gs Personality.default (current_player gs))));
  List.map (fun edge -> print_endline ((string_of_move edge.move)^" "^(string_of_float edge.heuristic))) tree.moves;
  (List.sort_uniq heuristic_compare tree.moves |> List.rev |> List.hd).move

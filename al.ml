open Move
open Game_state
open Board_state
open Heuristic

(** [move_edge] is the type of an edge in a [move_tree] with [move] that
    results in [outcomes] and a [heuristic]. *)
type move_edge = {
  move : move; 
  outcomes : move_tree list;
  heuristic : float;
}
(** [move_tree] is the type of a tree with nodes that have a [game_state], 
    [probability], and a list of [moves_edge] used for generating child 
    nodes. *)
and move_tree = {
  game_state : Game_state.t; 
  probability : float; 
  moves : move_edge list;
}

(** [move_edge_heuristic gs player move_trees personality] is the heuristic
    score of a [move_edge] with the [outcomes] [move_trees] for [player] with 
    [personality] in [gs]. Sums the heuristic score of each [move_tree] 
    in [move_trees] weighted with the [probability] of each 
    [move_tree]. *)
let rec move_edge_heuristic gs player move_trees personality =
  (** [internal tree] is the greatest heuristic score out of all the [mode_edge]
      in [moves] of [tree]. *)
  let internal {game_state;moves} = 
    List.map (fun edge -> move_edge_heuristic
                 game_state player edge.outcomes personality) moves |>
    List.sort Pervasives.compare |> List.rev |> List.hd in
  (** [heur tree] is the heuristic score of [tree]. *) (*TODO*)
  let heur tree = match tree.moves with
    | [] -> (*print_endline "GOT HERE";*)heuristic tree.game_state personality player
    | _ -> internal tree in
  (*print_endline ("\nmove edge heur: "^(string_of_float (List.fold_left 
                                                          (fun acc ({probability} as tree) ->
                                                             acc+.(probability*.(heur tree))) 0. move_trees)));*)
  List.fold_left 
    (fun acc ({probability} as tree) ->
       (*print_endline ("tree heur: "^(string_of_float (heur tree)));
         print_endline ("probability: "^(string_of_float (probability)));
         print_endline "";*)
       acc+.(probability*.(heur tree))) 0. move_trees

(** [attack_tree gs attacker defender armies] is a list of [move_tree] 
    resulting from [attacker] attacking [defender] with [armies] in [gs].

    Helper for [move_probabilities]. *)
let attack_tree gs attacker defender armies = 
  let total_attackers = (Board_state.node_army (board_st gs) attacker) - 1 in 
  let attack_armies = min total_attackers 3 in
  let total_defenders = Board_state.node_army (board_st gs) defender in
  let defend_armies = min total_defenders 2 in
  (** [attack_outcome gs a d] is the game state resulting from the attacking 
      node losing [a] armies the defending node losing [d] armies in [gs]. 
      Nearly identical to [Game_state.attack].*)
  let attack_outcome gs a d = 
    if d = total_defenders then begin 
      let board_state = Board_state.set_army 
          (Board_state.set_army 
             (Board_state.set_owner (board_st gs) defender 
                (node_owner (board_st gs) attacker)) defender 
             (armies - a)) attacker (total_attackers - armies + 1) in 
      let gs' = change_board_st gs board_state in 
      set_turn gs' (Attack (OccupyA (attacker,defender),true))
    end
    else begin
      let board_state = Board_state.set_army 
          (Board_state.set_army (board_st gs) defender 
             (total_defenders - d)) attacker 
          (total_attackers - a + 1) in
      let gs' = change_board_st gs board_state in
      set_turn gs' (Attack (AttackSelectA,false))
    end in
  (** [outcomes lst] is a list of [move_tree] with game states from 
      [attack_outcome], probabilities from the last element in each tuple in 
      [lst], and empty [moves]. *)
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
  | 3,1 -> outcomes [(1,0,0.3403);(0,1,0.6597)]
  | 3,2 -> outcomes [(2,0,0.2926);(1,1,0.3358);(0,2,0.3717)]
  | _ -> failwith "shouldn't happen"

(** [move_probabilities gs move] is a list of [move_tree] resulting from 
    applying [move] in [gs]. [moves] of each [move_tree] is empty. 

    Helper for [move_edge]. *)
let move_probabilities gs move =
  match (turn gs), move with
  | Pick _, PickM node
    -> [{game_state = pick_nodes gs node; probability = 1.; moves = []}]
  | Trade, TradeM stars
    -> [{game_state = trade_stars gs stars; probability = 1.; moves = []}]
  | Reinforce (SelectR, _), ReinforceM list
    -> [{game_state = List.fold_left
             (fun acc (node, army) -> reinforce acc node army) gs list;
         probability = 1.; moves = []}]
  | Attack (AttackSelectA,_), AttackM (attacker, defender, army)
    -> attack_tree gs attacker defender army
  | Attack (OccupyA (attacker, defender),_), OccupyM army
    -> [{game_state = occupy gs attacker defender army; probability = 1.; 
         moves = []}]
  | Fortify FromSelectF, FortifyM (from_node, to_node, army)
    -> [{game_state = fortify gs from_node to_node army; probability = 1.; 
         moves = []}]
  | (Attack (AttackSelectA,_) | Fortify FromSelectF), FinishM
    -> [{game_state = end_turn_step gs; probability = 1.; moves = []}]
  | _, FinishM -> [{game_state = gs; probability = 1.; moves = []}]
  | _ -> failwith ("invalid state/move combination: " ^ (string_of_move move))

(** [heuristic_compare e1 e2] compares the heuristic score of [e1] and [e2]
    using [Pervasives.compare]. *)
let heuristic_compare e1 e2 =
  Pervasives.compare e1.heuristic e2.heuristic

(** [first_n list n] is a list with the first [n] elements of [list]. *)
let first_n list n =
  let rec internal acc = function
    | _,0 | [],_ -> acc
    | hd :: tl, n' -> internal (hd :: acc) (tl, n' - 1)
  in internal [] (list, n) |> List.rev

(** [move_edge gs player personality depth move] is a [move_edge] with 
    [move], [outcomes] resulting from [player] with [personality] applying 
    [move] in [gs], and a [heuristic]. Populates [moves] in each [move_tree] in
    [outcomes] with the first [depth] best moves.

    Helper for [move_tree]. *)
let rec move_edge gs player personality depth move =
  let fill_moves move_tree =
    let all_moves = valid_moves move_tree.game_state
    in let pairs = List.map
           (fun move -> (move, heuristic move_tree.game_state
                           personality player)) all_moves
    in let sorted_moves = List.sort
           (fun (_, h1) (_, h2) -> Pervasives.compare h2 h1) pairs
                          |> List.map fst
    in let best_moves = sorted_moves(*first_n sorted_moves depth*)
    in {move_tree with moves = List.map
                           (move_edge move_tree.game_state player personality
                              (depth-1)) best_moves} in 
  let move_trees = match depth,(turn gs) with
    | 0,_ | _,Fortify _ | _,Pick _ -> move_probabilities gs move
    | _ -> List.map fill_moves (move_probabilities gs move)
  in (*print_endline ("calc move: "^(string_of_move move));*){move = move; outcomes = move_trees; 
                                                              heuristic = move_edge_heuristic gs player move_trees personality}

(** [move_tree gs probability player personality depth] is the [move_tree]
    with [probability] built from [depth] best moves out of all the valid moves 
    of [gs] [player] with [personality] can perform. *)
let move_tree gs probability player personality depth = 
  let edges = List.map (move_edge gs player personality depth) (valid_moves gs)  
  in {game_state = gs; probability = probability; moves = edges}

(** [best_move gs depth] is the [move] with the highest heuristic score
    that the current player of [gs] can perform. Looks [depth] moves ahead when 
    computing heuristic. *)
let best_move gs depth =
  let player = current_player gs
  in let tree = move_tree gs 1. player Personality.default depth in
  print_endline ("current gs heuristic "^
                 (string_of_float
                    (Heuristic.heuristic gs
                       Personality.default player)));
  ignore (List.map
            (fun edge -> print_endline
                ((string_of_move edge.move)^" "^
                 (string_of_float edge.heuristic))) tree.moves);
  (List.sort_uniq heuristic_compare tree.moves |> List.rev |> List.hd).move
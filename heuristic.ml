
open Board
open Board_state
open Game_state
open Move
open Personality

type score = float

let player_frontiers bs player =
  let predicate node = not
      (List.for_all
         (fun border -> node_owner bs border = Some player)
         (node_borders (board bs) node))
  in List.filter predicate (player_nodes bs player)

let player_frontier_opponents bs player node =
  let is_opponent node = node_owner bs node <> Some player
  in List.filter is_opponent (node_borders (board bs) node)

let player_frontier_opponent_max_army bs player node =
  0 :: begin List.map (node_army bs)
      (player_frontier_opponents bs player node)
             |> List.sort_uniq Pervasives.compare
  end |> List.rev |> List.hd

let player_heuristic bs player personality =
  let nodes = player_nodes bs player |> List.length
  in let armies = player_army bs player
  in let bonus = player_cont_bonus bs player
  in let regions = 1 (* TODO *)
  in let frontier_nodes = player_frontiers bs player
  in let frontiers = List.length frontier_nodes
  in let frontier_armies = List.fold_left
         (fun acc node -> acc + (node_army bs node)) 0 frontier_nodes
  in let frontier_differential = List.fold_left
         (fun acc node ->
            acc + (max ((player_frontier_opponent_max_army bs player node)
                        - (node_army bs node)) 0)) 0 frontier_nodes
  in node_heuristic personality nodes
     +. bonus_heuristic personality bonus
     +. army_heuristic personality armies
     +. region_heuristic personality regions
     +. frontier_heuristic personality frontiers
     +. frontier_armies_heuristic personality frontier_armies
     +. frontier_differential_heuristic personality frontier_differential

let rec heuristic gs personality player =
  let gs' = match turn gs with
    | Attack _ -> gs (*apply_move (end_turn_step gs)
                       (best_move_from_list gs (valid_fortifications gs) personality)*)
    | _ -> gs
  in player_heuristic (board_st gs') player personality
and best_move_from_list gs moves personality =
  List.map (fun move -> move, heuristic (apply_move gs move) personality (current_player gs)) moves
  |> List.sort_uniq (fun (_, s1) (_, s2) -> Pervasives.compare s1 s2)
  |> List.rev |> List.hd |> fst

let best_move gs personality =
  best_move_from_list gs (valid_moves gs) personality 

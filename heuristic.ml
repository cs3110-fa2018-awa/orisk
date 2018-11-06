
open Board
open Board_state
open Game_state
open Move
open Personality

type score = float

let player_frontier_opponents bs player node =
  let is_opponent node = node_owner bs node <> Some player
  in List.filter is_opponent (node_borders (board bs) node)

let player_frontier_opponent_max_army bs player node =
  0 :: begin List.map (node_army bs)
      (player_frontier_opponents bs player node)
             |> List.sort_uniq Pervasives.compare
  end |> List.rev |> List.hd

let player_heuristic bs personality player =
  let all_nodes = player_nodes bs player
  in let nodes = all_nodes |> List.length
  in let armies = player_army bs player
  in let bonus = player_cont_bonus bs player
  in let regions = 0 (* TODO *)
  in let frontier_nodes = player_frontiers bs player
  in let non_frontier_nodes = List.filter
         (fun node -> not (List.mem node frontier_nodes)) all_nodes
  in let frontiers = List.length frontier_nodes
  in let army_total node_list = List.fold_left
         (fun acc node -> acc + (node_army bs node)) 0 node_list
  in let frontier_armies = army_total frontier_nodes
  in let non_frontier_armies = army_total non_frontier_nodes
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
     +. non_frontier_armies_heuristic personality non_frontier_armies
     +. frontier_differential_heuristic personality frontier_differential

let heuristic gs personality player =
  let opponents = List.filter (fun p -> p <> player) (players gs)
  in let opponent_num = List.length opponents
  in let opponent_heuristics =
       List.map (player_heuristic (board_st gs) personality) opponents
  in let avg_opponent =
       List.fold_left (+.) 0. opponent_heuristics /. (float_of_int opponent_num)
  in let max_opponent = List.fold_left max 0. opponent_heuristics
  in let heuristic = player_heuristic (board_st gs) personality player
  in heuristic
     +. opponent_num_heuristic personality opponent_num
     +. max_opponent_heuristic personality max_opponent
     +. avg_opponent_heuristic personality avg_opponent

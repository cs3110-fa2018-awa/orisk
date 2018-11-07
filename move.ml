open Board
open Board_state
open Game_state

type move =
  | PickM of node_id
  | ReinforceM of (node_id * army) list
  | AttackM of node_id * node_id * army
  | OccupyM of army
  | FortifyM of node_id * node_id * army
  | FinishM

let string_of_move = function
  | PickM node -> "Pick " ^ node
  | ReinforceM list ->
    "Reinforce " ^
    (List.fold_left (fun acc (node, army) ->
         acc ^ node ^ " with " ^ (string_of_int army)) "" list)
  | AttackM (node1, node2, army) ->
    "Attack " ^ node2 ^ " from " ^ node1 ^ " with " ^ (string_of_int army)
  | OccupyM army -> "Occupy with " ^ (string_of_int army)
  | FortifyM (node1, node2, army) ->
    "Fortify " ^ node2 ^ " from " ^ node1 ^ " with " ^ (string_of_int army)
  | FinishM -> "Finish"

let apply_move gs move = match (turn gs), move with
  | Pick _, PickM node
    -> pick_nodes gs node
  | Reinforce (SelectR, _), ReinforceM list
    -> List.fold_left (fun acc (node, army) -> reinforce acc node army) gs list
  | Attack (AttackSelectA,_), AttackM (attacker, defender, army)
    -> let gs', _, _ = attack gs attacker defender army in gs'
  | Attack (OccupyA (attacker, defender),_), OccupyM army
    -> occupy gs attacker defender army
  | Fortify FromSelectF, FortifyM (from_node, to_node, army)
    -> fortify gs from_node to_node army
  | (Attack (AttackSelectA,_) | Fortify FromSelectF), FinishM
    -> end_turn_step gs
  | _, FinishM -> gs
  | _ -> failwith ("invalid state/move combination: " ^ (string_of_move move))

let range min max =
  let rec internal acc = function
    | n when n = min - 1 -> acc
    | n -> internal (n :: acc) (n - 1)
  in internal [] max

(** ['a String_map] is a map with keys of [node_id] or [cont_id]. *)
module String_map = Map.Make (String)

let valid_reinforcements gs remaining =
  let frontiers = player_frontiers (board_st gs) (current_player gs)
  in List.map (fun frontier -> ReinforceM [(frontier, remaining)]) frontiers

let valid_fortifications gs =
  let bs = board_st gs
  in let frontiers = player_frontiers (board_st gs) (current_player gs)
  in let moves_for_origin origin : move list =
       let min, max = 1, (node_army bs origin) - 1
       in List.map (fun target ->
           begin List.map (fun n ->
               FortifyM (origin, target, n)) (range min max)
           end) (dfs bs origin []
                 |> List.filter (fun target ->
                     target <> origin && List.mem target frontiers))
          |> List.flatten
  in FinishM :: begin
      List.map (fun node -> moves_for_origin node) (turn_valid_nodes gs)
      |> List.flatten end

let valid_moves gs : move list =
  let bs = board_st gs
  in let brd = board bs
  in match turn gs with
  | Pick _ -> List.map (fun node -> PickM node) (turn_valid_nodes gs)
  | Reinforce (SelectR, remaining) -> valid_reinforcements gs remaining
  | Attack (AttackSelectA,_)
    -> let moves_for_attacker attacker : move list =
         let army = min ((node_army bs attacker) - 1) 3
         in List.map (fun defender -> AttackM (attacker, defender, army))
           begin
             node_borders brd attacker |> List.filter
               (fun defender -> (node_owner bs defender <> node_owner bs attacker))
           end
    in FinishM :: begin
        List.map (fun node -> moves_for_attacker node) (turn_valid_nodes gs)
        |> List.flatten end
  | Attack (OccupyA (attacker, _),_)
    -> let min, max = 0, (node_army bs attacker) - 1
    in List.map (fun n -> OccupyM n) (range min max)
  | Fortify FromSelectF -> valid_fortifications gs
  | _ -> [FinishM]


open Board
open Board_state
open Game_state

type move =
  | PickM of node_id
  | ReinforceM of node_id * army
  | AttackM of node_id * node_id * army
  | OccupyM of army
  | Fortify of node_id * node_id * army
  | Finish

let apply_move gs move = match (turn gs), move with
  | Pick, PickM node
      -> pick_nodes gs node
  | Reinforce (SelectR, _), ReinforceM (node, army)
    -> reinforce gs node army
  | Attack AttackSelectA, AttackM (attacker, defender, army)
    -> let gs', _, _ = attack gs attacker defender army in gs'
  | Attack (OccupyA (attacker, defender)), OccupyM army
    -> occupy gs attacker defender army
  | Fortify FromSelectF, Fortify (from_node, to_node, army)
    -> fortify gs from_node to_node army
  | (Attack AttackSelectA | Fortify FromSelectF), Finish
    -> end_turn_step gs
  | _ -> failwith "invalid state/move combination"

let range min max =
  let rec internal acc = function
    | n when n = min - 1 -> acc
    | n -> internal (n :: acc) (n - 1)
  in internal [] max

let valid_moves gs : move list =
  let bs = board_st gs
  in let brd = board bs
  in match turn gs with
  | Pick -> List.map (fun node -> PickM node) (turn_valid_nodes gs)
  | Reinforce (SelectR, _) -> List.map (fun node -> ReinforceM (node, 1)) (turn_valid_nodes gs)
  | Attack AttackSelectA
    -> let moves_for_attacker attacker : move list =
         let army = (node_army bs attacker) - 1
         in List.map (fun defender -> AttackM (attacker, defender, army))
           begin
             node_borders brd attacker |> List.filter
               (fun defender -> (node_owner bs defender <> node_owner bs attacker))
           end
    in Finish :: begin
        List.map (fun node -> moves_for_attacker node) (turn_valid_nodes gs)
        |> List.flatten end
  | Attack (OccupyA (attacker, _))
    -> let min, max = 0, (node_army bs attacker) - 1
    in List.map (fun n -> OccupyM n) (range min max)
  | Fortify FromSelectF
    -> let moves_for_origin origin : move list =
         let min, max = 1, (node_army bs origin) - 1
         in List.map (fun target ->
             begin List.map (fun n ->
                 Fortify (origin, target, n)) (range min max)
             end) (dfs bs origin []) |> List.flatten
    in Finish :: begin
        List.map (fun node -> moves_for_origin node) (turn_valid_nodes gs)
        |> List.flatten end
  | _ -> failwith "todo"


open Board
open Board_state
open Game_state

type arrow = Up | Down | Left | Right

(** ['a String_map] is used for storing maps indexed by [node_id]
    or [cont_id], both of which are aliases of [string]. *)
module String_map = Map.Make (String)

type t = {
  game_state : Game_state.t;
  cursor_node : node_id;
  scroll : coords;
  move_map : ((arrow * node_id option) list) String_map.t;
  (* technically these would better as parameters of the state *)
  attacking_node : node_id option;
  from_fortify_node : node_id option;
  leaderboard : bool;
}

let game_state st = st.game_state

let board_state st = game_state st |> board_st

let board st = board_state st |> Board_state.board

let attacking_node st = st.attacking_node

let leaderboard st = st.leaderboard

let check_is_owner st (node:node_id option) =
  match node with 
  | None -> failwith "oops"
  | Some node_id 
    -> if node_owner (board_state st) node_id <> 
          Some (current_player st.game_state)
    then raise (NotOwner node_id) else () 

let change_attack_node st (node:node_id option) = 
  let () = check_is_owner st node in 
  {st with attacking_node = node; 
           game_state = set_turn st.game_state (Attack DefendSelectA)}

let from_fortify_node st = st.from_fortify_node

let change_from_fortify_node st node = 
  let () = check_is_owner st node in 
  {st with from_fortify_node = node;
           game_state = set_turn st.game_state (Fortify ToSelectF)}

let pi = acos (~-. 1.)

let target_angle arrow : float = match arrow with
  | Up -> ~-. (pi /. 2.)
  | Down -> (pi /. 2.)
  | Left -> pi
  | Right -> 0.

let angle_diff a b = atan2 (sin (b -. a)) (cos (b -. a)) |> abs_float

let find_best_move brd node arrow : (arrow * node_id option) =
  let target = target_angle arrow
  in (arrow, snd begin
      List.fold_left
        (fun ((prev, _) as acc : float * node_id option) (adj : node_id) ->
           begin
             let x, y = node_coords brd node
             in let ax, ay = node_coords brd adj
             in let theta =
                  atan2 (float_of_int (ay - y)) (float_of_int (ax - x))
             in let diff = angle_diff theta target
             in if diff < prev then (diff, Some adj) else acc
           end
        ) (pi /. 2., None) (node_borders brd node)
    end)

let build_move_list brd node : (arrow * node_id option) list =
  [
    find_best_move brd node Up;
    find_best_move brd node Down;
    find_best_move brd node Left;
    find_best_move brd node Right;
  ]

let build_move_map gs : ((arrow * node_id option) list) String_map.t =
  let brd = Game_state.board_st gs |> Board_state.board
  in fold_nodes brd
    (fun node acc -> String_map.add node (build_move_list brd node) acc)
    String_map.empty

let init gs =
  {
    game_state = gs;
    cursor_node = board_st gs |> Board_state.board |> nodes |> List.hd;
    scroll = (0, 0);
    move_map = build_move_map gs;
    attacking_node = None;
    from_fortify_node = None;
    leaderboard = false;
  }

let cursor st = node_coords (board st) st.cursor_node

let cursor_node st = st.cursor_node

let scroll st = st.scroll

let gs st gs =
  {st with game_state = gs}

let move_arrow (st : t) (arrow : arrow) =
  let lst = String_map.find st.cursor_node st.move_map
  in match List.assoc arrow lst with
  | Some node -> {st with cursor_node = node}
  | None -> st

let set_cursor_node st = function
  | None -> st
  | Some node_id -> {st with cursor_node = node_id} 

let toggle_leaderboard st = {st with leaderboard = not (leaderboard st)}

let pick st = 
  let new_st = {st with game_state = pick_nodes st.game_state st.cursor_node} in
  if List.mem None (new_st |> board_state |> owners) then new_st else
    {st with game_state = init_reinforce new_st.game_state}

let change_game_st st game_st = print_endline "wds68";
  {st with game_state = game_st;
           attacking_node = if List.mem (turn game_st) 
               [Attack DefendSelectA;Attack OccupyA] 
             then st.attacking_node else None;
           from_fortify_node = if List.mem (turn game_st) 
               [Fortify ToSelectF; Fortify CountF] 
             then st.from_fortify_node else None}

let extract = function
  | None -> failwith "extract failed"
  | Some x -> x

let turn_valid_nodes st =
  let gs = game_state st
  in let bs = board_state st
  in let b = board st
  in let is_owner = fun node -> node_owner bs node = Some (current_player gs)
  in let pred = match turn gs with
      | Null -> fun node -> node_owner bs node = None
      | Reinforce SelectR -> is_owner
      | Reinforce PlaceR -> failwith "shouldn't happen"
      | Attack AttackSelectA
        -> fun node -> node_owner bs node = Some (current_player gs)
                       && node_army bs node > 1
      | Attack DefendSelectA
        -> fun node -> node_owner bs node <> Some (current_player gs)
                       && st.attacking_node <> None
                       && List.mem node (node_borders b (extract st.attacking_node))
      | Attack OccupyA -> failwith "shouldn't happen"
      | Fortify FromSelectF -> fun node -> is_owner node
                                           && node_army bs node > 1
      | Fortify ToSelectF
        -> let reachable = match st.from_fortify_node with
            | None -> []
            | Some from -> dfs bs from []
        in fun node -> Some node <> st.from_fortify_node && List.mem node reachable
      | Fortify CountF -> failwith "shouldn't happen"
  in nodes_filter b pred

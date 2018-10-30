
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
  leaderboard : (bool * stats_category);
}

let game_state st = st.game_state

let board_state st = game_state st |> board_st

let board st = board_state st |> Board_state.board

let attacking_node st = match (turn st.game_state) with
  | Attack ((DefendSelectA node) | OccupyA (node,_)) -> Some node 
  | _ -> None

(** [leaderboard_on st] is whether or not the leaderboard is activated in [st]. *)
let leaderboard_on st = fst st.leaderboard

(** [leaderboard_cat st] is the category that the leaderboard is sorted by in [st]. *)
let leaderboard_cat st = snd st.leaderboard

(** [toggle_leaderboard st] is the interface with the leaderboard activation
    opposite of the one in [st]. *)
let toggle_leaderboard st = {st with leaderboard = (not (leaderboard_on st), leaderboard_cat st)}

(** [set_leaderboard_cat st cat] is the interface [st] with the sorted by category
    set to [cat]. *)
let set_leaderboard_cat st cat = {st with leaderboard = (leaderboard_on st, cat)}

let check_is_owner st (node:node_id option) =
  match node with 
  | None -> failwith "oops"
  | Some node_id 
    -> if node_owner (board_state st) node_id <> 
          Some (current_player st.game_state)
    then raise (NotOwner node_id) else () 

let change_attack_node st (node:node_id option) = 
  let () = check_is_owner st node in 
  match node with 
  | None -> st
  | Some n -> {st with game_state = set_turn st.game_state (Attack (DefendSelectA n))}

let from_fortify_node st = match (turn st.game_state) with 
  | Fortify ((ToSelectF node) | CountF (node,_)) -> Some node
  | _ -> None

let change_from_fortify_node st node = 
  let () = check_is_owner st node in 
  match node with 
  | None -> st
  | Some n -> {st with game_state = set_turn st.game_state (Fortify (ToSelectF n))}

let reinforce_place st node =
  let () = check_is_owner st node in
  match node with
  | None -> st
  | Some n -> 
    {st with game_state = 
               set_turn st.game_state 
                 (Reinforce ((PlaceR n),
                             remaining_reinforcements st.game_state))}

let fortify_select st node1 node2 = 
  let () = check_is_owner st node2 in
  match node1,node2 with 
  | Some n1, Some n2 
    -> {st with game_state = set_turn st.game_state (Fortify (CountF (n1,n2)))}
  | _ -> st

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
    leaderboard = (false, CatPlayer);
  }

let cursor st = node_coords (board st) st.cursor_node

let cursor_node st = st.cursor_node

let scroll st = st.scroll

let constrain num minim maxim =
  max (min num maxim) minim

let scroll_by st xscroll yscroll =
  let width, height = ANSITerminal.size () in
  let board_width = st |> board |> board_ascii_width in
  let board_height = st |> board |> board_ascii_height in
  {st with scroll = (constrain (x st.scroll + xscroll) 0 (board_width - width),
                     constrain (y st.scroll + yscroll) 0 (board_height - height))}

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

let pick st = {st with game_state = pick_nodes st.game_state st.cursor_node} 

let change_game_st st game_st = {st with game_state = game_st}

let turn_valid_nodes st =
  let gs = game_state st
  in let bs = board_state st
  in let b = board st
  in let is_owner = fun node -> node_owner bs node = Some (current_player gs)
  in let pred = match turn gs with
      | Null -> fun node -> node_owner bs node = None
      | Reinforce (SelectR,_) -> is_owner
      | Reinforce (PlaceR _,_) -> failwith "shouldn't happen"
      | Attack AttackSelectA
        -> fun node -> node_owner bs node = Some (current_player gs)
                       && node_army bs node > 1
      | Attack (DefendSelectA n)
        -> fun node -> node_owner bs node <> Some (current_player gs)
                       && List.mem node (node_borders b n)
      | Attack (OccupyA _) -> failwith "shouldn't happen"
      | Fortify FromSelectF -> fun node -> is_owner node
                                           && node_army bs node > 1
      | Fortify (ToSelectF n)
        -> let reachable = dfs bs n []
        in fun node -> node <> n && List.mem node reachable
      | Fortify (CountF _) -> failwith "shouldn't happen"
  in nodes_filter b pred

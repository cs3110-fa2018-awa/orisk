open Board
open Board_state
open Game_state

(** [arrow] is the type of arrow key direction. *)
type arrow = Up | Down | Left | Right

(** ['a String_map] is used for storing maps indexed by [node_id]
    or [cont_id], both of which are aliases of [string]. *)
module String_map = Map.Make (String)

(** [Interface.t] is the type representing the state of the game interface. 
    It includes the game state, which node the cursor is located at, scroll 
    coordinates, the map of nodes to adjacent nodes reached from arrow 
    directions, the leaderboard state, and the help state. *)
type t = {
  game_state : Game_state.t;
  cursor_node : node_id;
  scroll : coords;
  move_map : ((arrow * node_id option) list) String_map.t;
  leaderboard : (bool * stats_category);
  help : (bool * string);
}

(** [game_state st] is the game state corresponding to interface [st]. *)
let game_state st = st.game_state

(** [board_state st] is the board state corresponding to interface [st]. *)
let board_state st = game_state st |> board_st

(** [board st] is the board corresponding to interface [st]. *)
let board st = board_state st |> Board_state.board

(** [attacking_node st] is [Some node] that the current player is attacking or
    occupying from, or [None] if the current player is not selecting a defender
    or occupying. *)
let attacking_node st = match (turn st.game_state) with
  | Attack ((DefendSelectA node) | OccupyA (node,_)) -> Some node 
  | _ -> None

(** [leaderboard_on st] is whether or not the leaderboard
    is activated in [st]. *)
let leaderboard_on st = fst st.leaderboard

(** [leaderboard_cat st] is the category that the leaderboard
    is sorted by in [st]. *)
let leaderboard_cat st = snd st.leaderboard

(** [toggle_leaderboard st] is the interface with the leaderboard activation
    opposite of the one in [st]. *)
let toggle_leaderboard st = 
  {st with leaderboard = (not (leaderboard_on st), leaderboard_cat st)}

(** [set_leaderboard_cat st cat] is the interface [st]
    with the sorted by category set to [cat]. *)
let set_leaderboard_cat st cat = 
  {st with leaderboard = (leaderboard_on st, cat)}

(** [help_on st] is whether the help menu is activated in [st]. *)
let help_on st = fst st.help

(** [help_cat st] is the category of gameplay state in [st] that help 
    is being displayed for. *)
let help_cat st = snd st.help

(** [toggle_help st] is the interface with the help activation opposite
    of the one in [st]. *)
let toggle_help st = {st with help = (not (help_on st), help_cat st)}

(** [check_is_owner st node] is whether or not the current player owns
    [node] in [st]. *)
let check_is_owner st (node:node_id option) =
  match node with 
  | None -> failwith "Node does not exist"
  | Some node_id 
    -> if node_owner (board_state st) node_id <> 
          Some (current_player st.game_state)
    then raise (NotOwner node_id) else () 

(** [change_attack_node st node] is the interface [st] with the game state's 
    turn state set to selecting the defending node of the attack,
    with the attacking node [node]. *)
let change_attack_node st (node:node_id option) = 
  check_is_owner st node;
  match node with 
  | None -> st
  | Some n 
    -> {st with game_state = set_turn st.game_state (Attack (DefendSelectA n))}

(** [from_fortify_node st] is the node that the current player is fortifying
    from in interface [st]. *)
let from_fortify_node st = match (turn st.game_state) with 
  | Fortify ((ToSelectF node) | CountF (node,_)) -> Some node
  | _ -> None

(** [change_from_fortify_node st node] is the interface [st] with the game
    state's turn state set to selecting a node to fortify to,
    with the fortifying from node [node]. *)
let change_from_fortify_node st node = 
  check_is_owner st node; 
  match node with 
  | None -> st
  | Some n 
    -> {st with game_state = set_turn st.game_state (Fortify (ToSelectF n))}

(** [reinforce_place st node] is the interface [st] with the game state's turn
    state set to reinforce where the player can place armies onto [node]. *)
let reinforce_place st node =
  check_is_owner st node;
  match node with
  | None -> st
  | Some n -> 
    {st with game_state = 
               set_turn st.game_state 
                 (Reinforce ((PlaceR n),
                             remaining_reinforcements st.game_state))}

(** [fortify_select st node1 node2] is the interface [st] with the game state's
    turn state set to fortify where the player inputs the number of armies they
    want moved. *)
let fortify_select st node1 node2 = 
  check_is_owner st node2;
  match node1,node2 with 
  | Some n1, Some n2 
    -> {st with game_state = set_turn st.game_state (Fortify (CountF (n1,n2)))}
  | _ -> st

(** [pi] is the float approximation of the mathematical constant pi. *)
let pi = acos (~-. 1.)

(** [target_angle arrow] is the angle in radians corresponding to [arrow]. *)
let target_angle arrow : float = match arrow with
  | Up -> ~-. (pi /. 2.)
  | Down -> (pi /. 2.)
  | Left -> pi
  | Right -> 0.

(** [angle_diff a b] is the angle difference between angles [a] and [b],
    in radians. *)
let angle_diff a b = atan2 (sin (b -. a)) (cos (b -. a)) |> abs_float

(** [find_best_move board node arrow] is the tuple containing [arrow]
    and [Some n] if there exists an [n] adjacent to [node] that is
    in about the same direction from [node] as [arrow] in [board], or 
    [None] if such a node does not exist. *)
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

(** [build_move_list board node] is the list of all four possible directional 
    arrows from [node] and the nodes they point to in [board]. *)
let build_move_list brd node : (arrow * node_id option) list =
  [
    find_best_move brd node Up;
    find_best_move brd node Down;
    find_best_move brd node Left;
    find_best_move brd node Right;
  ]

(** [build_move_map gamestate] is the map of all nodes in [gamestate] to their
    possible directional movements and the nodes the movements lead to. *)
let build_move_map gs : ((arrow * node_id option) list) String_map.t =
  let brd = Game_state.board_st gs |> Board_state.board
  in fold_nodes brd
    (fun node acc -> String_map.add node (build_move_list brd node) acc)
    String_map.empty

(** [init gs] is the initial interface with game state [gs]. *)
let init gs =
  {
    game_state = gs;
    cursor_node = board_st gs |> Board_state.board |> nodes |> List.hd;
    scroll = (0, 0);
    move_map = build_move_map gs;
    leaderboard = (false, CatPlayer);
    help = (true, "pick")
  }

(** [cursor st] is the coordinates of the cursor in interface [st]. *)
let cursor st = node_coords (board st) st.cursor_node

(** [cursor_node st] is the node that the cursor is at in interface [st]. *)
let cursor_node st = st.cursor_node

(** [scroll st] is the coordinates of scroll in interface [st]. *)
let scroll st = st.scroll

(** [constrain n min max] bounds [n] between [min] and [max]. *)
let constrain num minim maxim =
  max (min num maxim) minim

(** [scroll_by st xscroll yscroll] is the interface [st] with the ASCII map in 
    the board display scrolled horizontally by [xscroll] and vertically by 
    [yscroll]. *)
let scroll_by st xscroll yscroll =
  let width, height = ANSITerminal.size () in
  let board_width = st |> board |> board_ascii_width in
  let board_height = st |> board |> board_ascii_height in
  {st with scroll = (constrain (x st.scroll + xscroll) 0 (board_width - width),
                     constrain 
                       (y st.scroll + yscroll) 0 (board_height - height + 3))}

(** [gs st gs] is the interface [st] with game state [gs]. *)
let gs st gs =
  {st with game_state = gs}

(** [move_arrow st arrow] is the interface [st] with the cursor moved to the
    destination node corresponding with [arrow] from the current cursor node
    in [st]. *)
let move_arrow (st : t) (arrow : arrow) =
  let lst = String_map.find st.cursor_node st.move_map
  in match List.assoc arrow lst with
  | Some node -> {st with cursor_node = node}
  | None -> st

(** [set_cursor_node st node] is the interface [st] with the cursor moved to 
    [node]. *)
let set_cursor_node st = function
  | None -> st
  | Some node_id -> {st with cursor_node = node_id} 

(** [pick st] is the interface [st] with the game state with the cursor node
    selected to be owned by the current player. *)
let pick st = {st with game_state = pick_nodes st.game_state st.cursor_node} 

(** [change_game_st st gamestate] is the interface with 
    the game state [gamestate].*)
let change_game_st st game_st = {st with game_state = game_st}

(** [turn_valid_nodes st] is the list of nodes that are able to be actioned
    upon during the current game state in interface [st]. *)
let turn_valid_nodes st =
  let gs = game_state st
  in let bs = board_state st
  in let b = board st
  in let is_owner = fun node -> node_owner bs node = Some (current_player gs)
  in let pred = match turn gs with
      | Pick -> fun node -> node_owner bs node = None
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

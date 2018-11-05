open Board
open Player

(** ['a String_map] is a map with keys of [node_id] or [cont_id]. *)
module String_map = Map.Make (String)
(** ['a String_set] is a set with keys of [node_id] or [cont_id]. *)
module String_set = Set.Make (String)
(** ['a Player_map] is a map with keys of [Player.t]. *)
module Player_map = Map.Make (Player)

(* idea: we want to be able to access this information quickly
   from both ends; so we implement data structures that facilitate
   both sides of data access and update them at the same time *)

(** [node_state] is the state of a node, used internally.
    Owner (may be none to represent not owned by anyone) and army. *)
type node_state = {owner : Player.t option; army : army}

(** [cont_state] is the state of a continent, used internally.
    Owner (may by none to represent not owned by anyone). *)
type cont_state = {owner : Player.t option}

(** [player_state] is the state of a player, used internally.
    Set of nodes and continents owned by the player. *)
type player_state = {nodes : String_set.t; conts : String_set.t; stars : int}

(** [player_stats] is the board statistics of a player.
    It contains the total number of armies, territories, and continents
    that a player owns. *)
type player_stats = {
  player : Player.t; 
  army_tot : army; 
  node_tot : int; 
  cont_tot : int
}

(** [stats_category] is the category that the board leaderboard can be 
    sorted by. *)
type stats_category = CatPlayer | CatArmy | CatNode | CatCont

(** [Board_state.t] is the state of a board. The underlying (and
    unchanging) board, the map of nodes to node states, the map
    of continents to continent states, and the map of players
    to player states. *)
type t = {
  board : Board.t;
  nodes : node_state String_map.t;
  conts : cont_state String_map.t;
  players : player_state Player_map.t;
}

(** [UnknownPlayer player] is the exception raised when a unknown player
    ID is specified. *)
exception UnknownPlayer of Player.t

(** [init b players] is the default state from board [b].
    All nodes have no owner and zero armies. All continents have no
    owner. All players have no nodes and no continents. *)
let init board players =
  {
    board = board;
    nodes = fold_nodes board
        (fun node_id acc -> String_map.add node_id
            {owner = None; army = 0} acc)
        String_map.empty;
    conts = fold_conts board
        (fun cont_id acc -> String_map.add cont_id
            {owner = None} acc)
        String_map.empty;
    players = List.fold_left
        (fun acc player -> Player_map.add player
            {nodes = String_set.empty; conts = String_set.empty; stars = 0} acc)
        Player_map.empty players;
  }

(** [board st] is the board used by state [s]. *)
let board st = st.board

(** [node_state st node] is the state of the node
    referenced by [node] in [st]. *)
let node_state st node_id =
  match String_map.find_opt node_id st.nodes with
  | Some (state) -> state
  | None -> raise (UnknownNode node_id)

(** [cont_state st cont] is the state of the continent
    referenced by [cont] in [st]. *)
let cont_state st cont_id =
  match String_map.find_opt cont_id st.conts with
  | Some (state) -> state
  | None -> raise (UnknownCont cont_id)

(** [player_state st player] is the state of the player
    referenced by [player] in [st]. *)
let player_state st player =
  match Player_map.find_opt player st.players with
  | Some (state) -> state
  | None -> raise (UnknownPlayer player)

(** [node_owner state id] is [Some player] if node [id] is owned by 
    [player], or [None] if [id] is not owned by anyone. *)
let node_owner st node = (node_state st node).owner

(** [owners state] is the list of players that control at least one
    node in [state]. *)
let owners st =
  String_map.fold (fun k (d : node_state) acc -> d.owner :: acc) st.nodes []

(** [node_army state id] is the army stationed at node [id] in [state]. *)
let node_army st node = (node_state st node).army

(** [cont_owner state id] is [Some player] if continent [id] 
    is owned by [player], or [None] if [id] is not owned by anyone. *)
let cont_owner st cont = (cont_state st cont).owner

(** [player_nodes state player] is a list of the nodes
    owned by [player] in [state]. *)
let player_nodes st player =
  String_set.elements (player_state st player).nodes

(** [player_conts state player] is a list of the continents
    owned by [player] in [state]. *)
let player_conts st player =
  String_set.elements (player_state st player).conts

(** [player_army state player] is the total number of armies owned
    by [player] in [state]. This function performs the calculation
    as this information is not saved in the board state. *)
let player_army st (player:Player_map.key) : army =
  List.fold_left (fun acc node -> acc + (node_army st node))
    0 (player_nodes st player)

let player_stars st player : int =
  (player_state st player).stars

(** [stats_player ps] is the player in [ps]. *)
let stats_player ps = ps.player

(** [stats_army ps] is the total armies owned by a player in [ps]. *)
let stats_army ps = ps.army_tot

(** [stats_nodes ps] is the total territories owned by a player in [ps]. *)
let stats_nodes ps = ps.node_tot

(** [stats_conts ps] is the total continents owned by a player in [ps]. *)
let stats_conts ps = ps.cont_tot

(** [get_players state] is the list of all existing players in [state]. *)
let get_players st : Player.t list = 
  Player_map.fold (fun key _ acc ->
      if List.mem key acc then acc else key :: acc) st.players []

(** [player_stats_make state player] is the data structure containing the total
    number of territories, continents, and armies (in that order) owned by
    [player] in [state]. *)
let player_stats_make st p : player_stats =
  let n = player_nodes st p |> List.length in
  let c = player_conts st p |> List.length in
  let a = player_army st p in
  {player = p; army_tot = a; node_tot = n; cont_tot = c}

(*BISECT-IGNORE-BEGIN*) (*play test*)
(** [compare_player_stats category ps1 ps2] is a comparison function
    (similar to Pervasives.compare) that accounts for each field in a record of
    [player_stats], based on [category]. It will result in sorting players in
    ascending order and armies, territories, and continents in descending order. 
*)
let compare_player_stats (c : stats_category) ps1 ps2 : int = match c with 
  | CatPlayer -> Player.compare ps1.player ps2.player 
  | CatArmy -> - Pervasives.compare ps1.army_tot ps2.army_tot
  | CatNode -> - Pervasives.compare ps1.node_tot ps2.node_tot
  | CatCont -> - Pervasives.compare ps1.cont_tot ps2.cont_tot

(** [sorted_player_stats state category] is the list of all player statistics,
    sorted based on [category] in [state]. *)
let sorted_player_stats (c : stats_category) st : player_stats list =
  let lst = 
    List.fold_left 
      (fun acc player -> (player_stats_make st player)::acc) [] (get_players st)
  in List.sort (compare_player_stats c) lst
(*BISECT-IGNORE-END*)

(** [extract ex a] extracts the value from the option [a]
    if that option is [Some value] and raises [ex] otherwise. *)
let extract except (a : 'a option) =
  match a with
  | Some x -> x
  | None -> raise except (*BISECT-IGNORE*) (*helper function not in mli*)

(** [player_reinforcements state player] is the total number of
    reinforcements that [player] recieves given the current board
    configuration. This includes reinforcements from the number
    of nodes ([max(floor(n/3),3)]) and the sum of all bonuses
    provided by controlling entire continents. *)
let player_reinforcements st player =
  (* territory reinforcements *)
  (max (List.length (player_nodes st player) / 3) 3)
  (* continent bonus *)
  + (List.fold_left (fun acc cont_id ->
      acc + (Board.cont_bonus st.board cont_id))
      0 (player_conts st player))

(** TODO *)
let set_stars st player stars =
  let ({players}:t) = st in 
  let new_player_st = fun (state:player_state option) -> 
    Some {(extract (UnknownPlayer player) state) with stars = stars} in 
  {st with players = Player_map.update player new_player_st players}

(** TODO *)
let place_stars st player stars =
  set_stars st player ((player_stars st player) + stars)

(** [set_army state node army] is the new state resulting from setting
    [node] to have [army] armies in [state]. *)
let set_army st node army =
  let ({nodes} : t) = st
  in let new_node_st = fun state ->
      Some {(extract (UnknownNode node) state) with army = army}
  in {st with nodes = String_map.update node new_node_st nodes}

(** [place_army state node army] is the new state resulting from adding
    [army] armies to [node] in [state]. This is a helper function
    that merely calls [set_army] internally. *)
let place_army st node army =
  set_army st node ((node_army st node) + army)

(** [player_color_from_node st node] is the option of the color of [node]
    in [st]. This is [Some color] if the owner of the node is [Some player]
    and [None] if the owner of the node is [None]. *)
let player_color_from_node (st : t) (node_id : Board.node_id) = 
  match (node_owner st node_id) with
  | Some p -> Some (Player.player_color p)
  | None -> None

(** [dfs node visited] is a special implementation of a depth first search that
    will only go along monochromatic paths. 
    Returns a list of nodes visited. *)
let rec dfs (st : t) (node : node_id) (visited : node_id list) : node_id list =
  let internal lst n =
    if (node_owner st n) = (node_owner st node) && not (List.mem n lst)
    then dfs st n (n :: lst) else lst
  in let filter (n : node_id) = not (List.mem n visited)
  in List.fold_left 
    internal visited (node_borders (board st) node |> List.filter filter)

(** [update_map player_opt f map] runs [Map.replace] on [map]
    with option handling for [player_opt] and [f]. *)
let update_map
    (player_opt : Player.t option)
    (f : player_state -> player_state)
    (map : player_state Player_map.t) =
  let replacer player'' state_opt =
    Some (f (extract (UnknownPlayer player'') state_opt))
  in match player_opt with
  | Some player' -> Player_map.update player' (replacer player') map
  | None -> map

(** [is_owner st player node cont] is true iff [player] is the owner of [cont]
     after owning the target [node] in state [st]. *)
let is_owner st player node cont =
  List.for_all
    (fun n -> (n = node) || ((node_owner st n) = player))
    (cont_nodes (board st) cont)

(** [star_generator ()] is either 1 or 2 stars. *)
let star_generator = 
  fun () ->
    Random.self_init ();
    let p = 0.08 in
    if (Random.float 1.0) > p then 1 else 2

(** [set_owner state node player] is the new state resulting from
    changing ownership of [node] to [player] in [state].

    This function needs to accomplish several things:
     - change owner of [node] to [player]
     - remove old owner from continents containing [node]
     - add [player] as owner of continents containing [node]
         newly controlled by [player] as a result
     - update controlled node and cont lists in player state *)
let set_owner (st : t) (node : node_id) (player : Player.t option) =
  (* the previous owner of the target node *)
  let prev_owner = node_owner st node
  (* all continents containing the target node *)
  in let node_conts = node_conts (board st) node
  (* the new state of the target node, with the owner updated *)
  in let new_node_st = fun state ->
      Some ({(extract (UnknownNode node) state)
             with owner = player} : node_state)

  (* update state of new owner *)
  in let new_player_st
      ({nodes=nodes'; conts=conts'; stars=stars'} : player_state) : player_state =
       {
         (* add node to list of controlled nodes *)
         nodes = String_set.add node nodes';
         (* add continents that the player now fully controls *)
         conts = List.fold_left
             (fun acc cont -> if is_owner st player node cont
               then String_set.add cont acc else acc)
             conts' node_conts;
         stars = stars' + star_generator ()
       }

  (* update state of previous owner *)
  in let prev_player_st
      ({nodes=nodes'; conts=conts'; stars=stars'} : player_state) : player_state =
       {
         (* remove node from list of controlled nodes *)
         nodes = String_set.remove node nodes';
         (* remove continents that the player no longer controls *)
         conts = List.fold_left
             (fun acc cont -> String_set.remove cont acc)
             conts' node_conts;
         stars=stars'
       }

  (* make player owner of newly controlled continents and
     remove owner from continents no longer controlled by
     previous owner *)
  in let new_conts conts' player' = List.fold_left
         (fun acc cont ->
            String_map.update cont (fun cont_st_opt ->
                if is_owner st player node cont then Some {owner = player'}
                else None) conts'
         ) conts' node_conts

  (* state transition *)
  in let ({nodes; conts; players} : t) = st in
  {
    st with
    nodes = String_map.update node new_node_st nodes;
    conts = new_conts conts player;
    players = update_map player new_player_st players
              |> update_map prev_owner prev_player_st;
  }
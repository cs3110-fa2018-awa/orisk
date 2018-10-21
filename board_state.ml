
open Board
open Player

module String_map = Map.Make (String)
module String_set = Set.Make (String)
module Player_map = Map.Make (Player)

type army = int

(* idea: we want to be able to access this information quickly
   from both ends; so we implement data structures that facilitate
   both sides of data access and update them at the same time *)

type node_state = {owner : Player.t option; army : army}
type cont_state = {owner : Player.t option}
type player_state = {nodes : String_set.t; conts : String_set.t}

type t = {
  board : Board.t;
  nodes : node_state String_map.t;
  conts : cont_state String_map.t;
  players : player_state Player_map.t;
}

exception UnknownPlayer of Player.t

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
            {nodes = String_set.empty; conts = String_set.empty} acc)
        Player_map.empty players;
  }

let board ({board} : t) = board

let node_state ({nodes} : t) node_id =
  match String_map.find_opt node_id nodes with
  | Some (state) -> state
  | None -> raise (UnknownNode node_id)

let cont_state ({conts} : t) cont_id =
  match String_map.find_opt cont_id conts with
  | Some (state) -> state
  | None -> raise (UnknownCont cont_id)

let player_state ({players} : t) player =
  match Player_map.find_opt player players with
  | Some (state) -> state
  | None -> raise (UnknownPlayer player)

let node_owner st node =
  let ({owner} : node_state) = (node_state st node) in owner

let node_army st node =
  let ({army} : node_state) = (node_state st node) in army

let cont_owner st cont =
  let ({owner} : cont_state) = (cont_state st cont) in owner

let player_nodes st player =
  let ({nodes} : player_state) = (player_state st player)
  (* TODO this might be inefficient *)
  in String_set.elements nodes

let player_conts st player =
  let ({conts} : player_state) = (player_state st player)
  (* TODO this might be inefficient *)
  in String_set.elements conts

let player_army st player : army =
  List.fold_left (fun acc node -> acc + (node_army st node))
    0 (player_nodes st player)

let extract except (a : 'a option) =
  match a with
  | Some x -> x
  | None -> raise except

let set_army st node army =
  let ({nodes} : t) = st
  in let new_node_st = fun state ->
    Some {(extract (UnknownNode node) state) with army = army}
  in {st with nodes = String_map.update node new_node_st nodes}

let place_army st node army =
  set_army st node ((node_army st node) + army)

(** [set_owner state node player] needs to accomplish several things:
     - change owner of [node] to [player]
     - remove old owner from continents containing [node]
     - add [player] as owner of continents containing [node]
         newly controlled by [player] as a result
     - update controlled node and cont lists in player state
*)
let set_owner (st : t) (node : node_id) (player : Player.t option) =
  (* this is a giant state transition function *)

  let update_map
      (player_opt : Player.t option)
      (f : player_state -> player_state)
      (map : player_state Player_map.t) =
    let replacer player'' state_opt =
      Some (f (extract (UnknownPlayer player'') state_opt))
    in match player_opt with
    | Some player' -> Player_map.update player' (replacer player') map
    | None -> map

  in let prev_owner = node_owner st node
  in let node_conts = node_conts (board st) node
  in let new_node_st = fun state ->
      Some ({(extract (UnknownNode node) state)
             with owner = player} : node_state)

  in let is_owner cont =
       List.for_all
         (fun n -> (n = node) || ((node_owner st n) = player))
         (cont_nodes (board st) cont)

  (* update state of new owner *)
  in let new_player_st
      ({nodes=nodes'; conts=conts'} : player_state) : player_state =
       {
         (* add node to list of controlled nodes *)
         nodes = String_set.add node nodes';
         (* add continents that the player now fully controls *)
         conts = List.fold_left
             (fun acc cont -> if is_owner cont
               then String_set.add cont acc else acc)
             conts' node_conts
       }

  (* update state of previous owner *)
  in let prev_player_st
      ({nodes=nodes'; conts=conts'} : player_state) : player_state =
       {
         (* remove node from list of controlled nodes *)
         nodes = String_set.remove node nodes';
         (* remove continents that the player no longer controls *)
         conts = List.fold_left
             (fun acc cont -> String_set.remove cont acc)
             conts' node_conts
       }

  (* make player owner of newly controlled continents and
     remove owner from continents no longer controlled by
     previous owner *)
  in let new_conts conts' player' = List.fold_left
         (fun acc cont ->
            String_map.update cont (fun cont_st_opt ->
                if is_owner cont then
                  (* this gives us a warning now,
                     but we may want to be able to add
                     additional record fields later *)
                  Some {(extract (UnknownCont cont) cont_st_opt)
                        with owner = player'}
                else None) conts'
         ) conts' node_conts

  in let ({nodes; conts; players} : t) = st in
  {
    st with
    nodes = String_map.update node new_node_st nodes;
    conts = new_conts conts player;
    players = update_map player new_player_st players
              |> update_map prev_owner prev_player_st;
  }


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
}

let game_state st = st.game_state

let board_state st = game_state st |> board_st

let board st = board_state st |> Board_state.board

let pi = acos (~-. 1.)

let target_angle arrow : float = match arrow with
  | Up -> ~-. (pi /. 2.)
  | Down -> (pi /. 2.)
  | Left -> pi
  | Right -> 0.

let angle_diff a b =
  let res = abs_float ((mod_float (a -. b +. pi) (pi *. 2.)) -. pi)
  in print_endline ("angle_diff " ^ (string_of_float a) ^ " " ^ (string_of_float b) ^ " = " ^ (string_of_float res)); res

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
             in if diff < prev
             then (diff, Some adj) else acc
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

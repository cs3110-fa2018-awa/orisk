
open Board
open Board_state
open Game_state

type t = {
  game_state : Game_state.t;
  cursor : coords;
  cursor_node : node_id;
  scroll : coords
}

let init gs =
  let brd = board_st gs |> Board_state.board in
  let default_node = brd |> nodes |> List.hd in
  {
    game_state = gs;
    cursor = node_coords brd default_node;
    cursor_node = default_node;
    scroll = (0, 0);
  }

let game_state st = st.game_state

let board_state st = game_state st |> board_st

let board st = board_state st |> Board_state.board

let cursor st = st.cursor

let cursor_node st = st.cursor_node

let scroll st = st.scroll


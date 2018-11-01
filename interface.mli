open Board
open Board_state

(** [Interface.t] is the abstract type representing a game interface. *)
type t

(** [arrow] is the type of arrow key direction. *)
type arrow = Up | Down | Left | Right

(** [init gs] is the initial interface with game state [gs]. *)
val init : Game_state.t -> t

(** [game_state st] is the game state corresponding to interface [st]. *)
val game_state : t -> Game_state.t

(** [board_state st] is the board state corresponding to interface [st]. *)
val board_state : t -> Board_state.t

(** [board st] is the board corresponding to interface [st]. *)
val board : t -> Board.t

(** [leaderboard_on st] is whether or not the leaderboard 
    is activated in [st]. *)
val leaderboard_on : t -> bool

(** [leaderboard_cat st] is the category that the leaderboard 
    is sorted by in [st]. *)
val leaderboard_cat : t -> stats_category

(** [toggle_leaderboard st] is the interface with the leaderboard activation
    opposite of the one in [st]. *)
val toggle_leaderboard : t -> t

(** [set_leaderboard_cat st cat] is the interface [st] with the sorted by 
    category set to [cat]. *)
val set_leaderboard_cat : t -> stats_category -> t

(** [help_on st] is whether the help menu is activated in [st]. *)
val help_on : t -> bool

(** [help_cat st] is the category of gameplay state in [st] that help 
    is being displayed for. *)
val help_cat : t -> string

(** [toggle_help st] is the interface with the help activation opposite
    of the one in [st]. *)
val toggle_help : t -> t

(** [attacking_node st] is [Some node] that the current player is attacking or
    occupying from, or [None] if the current player is not selecting a defender
    or occupying. *)
val attacking_node : t -> node_id option

(** [leaderboard_on st] is whether or not the leaderboard 
    is activated in [st]. *)
val change_attack_node : t -> node_id option -> t

(** [from_fortify_node st] is the node that the current player is fortifying
    from in interface [st]. *)
val from_fortify_node : t -> node_id option

(** [change_from_fortify_node st node] is the interface [st] with the game
    state's turn state set to selecting a node to fortify to,
    with the fortifying from node [node]. *)
val change_from_fortify_node : t -> node_id option -> t

(** [reinforce_place st node] is the interface [st] with the game state's turn
    state set to reinforce where the player can place armies onto [node]. *)
val reinforce_place : t -> node_id option -> t

(** [fortify_select st node1 node2] is the interface [st] with the game state's
    turn state set to fortify where the player inputs the number of armies they
    want moved. *)
val fortify_select : t -> node_id option -> node_id option -> t

(** [cursor st] is the coordinates of the cursor in interface [st]. *)
val cursor : t -> coords

(** [cursor_node st] is the node that the cursor is at in interface [st]. *)
val cursor_node : t -> node_id

(** [scroll st] is the coordinates of scroll in interface [st]. *)
val scroll : t -> coords

(** [scroll_by st xscroll yscroll] is the interface [st] with the ASCII map in 
    the board display scrolled horizontally by [xscroll] and vertically by 
    [yscroll]. *)
val scroll_by : t -> int -> int -> t

(** [gs st gs] is the interface [st] with game state [gs]. *)
val gs : t -> Game_state.t -> t

(** [move_arrow st arrow] is the interface [st] with the cursor moved to the
    destination node corresponding with [arrow] from the current cursor node
    in [st]. *)
val move_arrow : t -> arrow -> t

(** [set_cursor_node st node] is the interface [st] with the cursor moved to 
    [node]. *)
val set_cursor_node : t -> node_id option -> t

(** [pick st] is the interface [st] with the game state with the cursor node
    selected to be owned by the current player. *)
val pick : t -> t

(** [change_game_st st gamestate] is the interface with 
    the game state [gamestate].*)
val change_game_st : t -> Game_state.t -> t

(** [turn_valid_nodes st] is the list of nodes that are able to be actioned
    upon during the current game state in interface [st]. *)
val turn_valid_nodes : t -> node_id list

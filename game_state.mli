open Player
open Board_state
open Board

type turn_state = Reinforce | Attack 

type players = Player.t list 

type t

exception NoPlayers
exception NonadjacentNode of (node_id * node_id)
exception InvalidState of turn_state
exception InsufficientArmies of (node_id * army)
exception FriendlyFire of Player.t option

val init : Board.t -> players -> t

val board_st : t -> Board_state.t

val turn : t -> turn_state

val attack : t -> node_id -> node_id -> Board_state.army -> t

val reinforce : t -> node_id -> t
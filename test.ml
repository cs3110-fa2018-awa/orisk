open OUnit2
open ANSITerminal
open Board
open Player
open Board_state
open Game_state

(* General note to all those concerned.

   I am using laziness here so that unexpected exceptions raised by
   functions in tests will be traced correctly to the test that
   caused them. This approach lets us use the helper functions
   [gen_comp] and [except_comp] instead of making anonymous functions
   for every test.

   You use this by making every [actual] in a test a [lazy 'a]. If
   an [actual] requires the use of a previously defined [actual], then
   the function [(~$)] (shorthand for [Lazy.force]) can be used to
   expand the lazy within another lazy. *)

(* for brevity *)
let (~$) = Lazy.force

(** [gen_comp test_name actual expected printer] compares [actual] and 
    [expected] and prints with [printer]. *)
let gen_comp
    (test_name : string)
    (actual : 'a Lazy.t)
    (expected : 'a)
    (printer : 'a->string) =
  test_name >::
  (fun _ -> assert_equal expected (~$ actual) ~printer:printer)

(** [except_comp test_name actual except] tests to see that forcing
    [actual] produces exception [except]. *)
let except_comp
    (test_name : string)
    (actual : 'a Lazy.t)
    except =
  test_name >:: (fun _ -> assert_raises except (fun () -> (~$ actual)))

(* printers *)
let null _ = ""
let str s = s
let bool = string_of_bool
let int = string_of_int
let coord (x, y) = "{x: " ^ (int x) ^ "; y: " ^ (int y) ^ "}"
let player_p player = Player.player_name player
let opt p = function
  | Some x -> p x
  | None -> "none"

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(* test boards *)
let map_schema = lazy (from_json (Yojson.Basic.from_file "mapSchema.json"))

let ascii = "............XX..\n..........//....\n.........//.....\n........XX==XX..\n............||..\n............XX..\n....XX==XX......\n................"

let board_tests = [
  gen_comp "board name" (lazy (board_name (~$ map_schema))) "Cornell" str;
  gen_comp "board ascii" (lazy (board_ascii (~$ map_schema))) ascii str;
  gen_comp "nodes" (lazy (nodes (~$ map_schema)))
    (List.sort Pervasives.compare ["RPCC"; "JAM"; "LR7"; "HR5"; "Keeton"; "Rose"]) (pp_list str);
  gen_comp "has node" (lazy (has_node (~$ map_schema) "RPCC")) true bool;
  gen_comp "doesn't have node" (lazy (has_node (~$ map_schema) "foo")) false bool;
  gen_comp "node name" (lazy (node_name (~$ map_schema) "JAM")) "Just About Music" str;
  except_comp "invalid node" (lazy (node_name (~$ map_schema) "foo")) (UnknownNode "foo");
  gen_comp "node coords" (lazy (node_coords (~$ map_schema) "RPCC")) (4, 3) coord;
  gen_comp "node borders" (lazy (node_borders (~$ map_schema) "JAM"))
    (List.sort Pervasives.compare ["LR7"]) (pp_list str);
  gen_comp "conts" (lazy (conts (~$ map_schema)))
    (List.sort Pervasives.compare ["North"; "West"]) (pp_list str);
  gen_comp "has cont" (lazy (has_cont (~$ map_schema) "North")) true bool;
  gen_comp "doesn't have cont" (lazy (has_cont (~$ map_schema) "foo")) false bool;
  gen_comp "cont name" (lazy (cont_name (~$ map_schema) "North")) "North Campus" str;
  except_comp "invalid cont" (lazy (cont_name (~$ map_schema) "foo")) (UnknownCont "foo");
  gen_comp "cont bonus" (lazy (cont_bonus (~$ map_schema) "North")) 5 int;
  gen_comp "cont nodes" (lazy (List.sort Pervasives.compare (cont_nodes (~$ map_schema) "North")))
    (List.sort Pervasives.compare ["RPCC"; "JAM"; "LR7"; "HR5"]) (pp_list str);
]

let player_a = lazy (Player.create "player_a" Red)
let player_b = lazy (Player.create "player_b" Green)
let player_c = lazy (Player.create "player_c" Blue)

let player_tests = [
  gen_comp "player name" (lazy (player_name (~$ player_a))) "player_a" str;
  gen_comp "player color" (lazy (player_color (~$ player_a))) Red null;
]

let demo_players = lazy [
  ~$ player_a;
  ~$ player_b;
  ~$ player_c;
]

let false_player = lazy (Player.create "foo" Black)

let init_board_state = lazy (Board_state.init (~$ map_schema) (~$ demo_players))

let set_armies_state = lazy (set_army (~$ init_board_state) "JAM" 2)
let add_armies_state = lazy (place_army (place_army (~$ set_armies_state) "RPCC" 5) "JAM" 2)

let player_a_own_rpcc = lazy (set_owner (~$ add_armies_state) "RPCC" (Some (~$ player_a)))
let player_a_own_rpcc_jam = lazy (set_owner (~$ player_a_own_rpcc) "JAM" (Some (~$ player_a)))

let board_state_tests = [
  (* initial board state *)
  gen_comp "board state board"
    (lazy (board (~$ init_board_state))) (~$ map_schema) null;
  gen_comp "board state init node owner"
    (lazy (node_owner (~$ init_board_state) "RPCC")) None (opt player_p);
  gen_comp "board state init node army"
    (lazy (node_army (~$ init_board_state) "RPCC")) 0 int;
  gen_comp "board state init cont owner"
    (lazy (cont_owner (~$ init_board_state) "North")) None (opt player_p);
  gen_comp "board state init player nodes"
    (lazy (player_nodes (~$ init_board_state) (~$ player_a))) [] (pp_list str);
  gen_comp "board state init player conts"
    (lazy (player_conts (~$ init_board_state) (~$ player_a))) [] (pp_list str);
  gen_comp "board state init player army"
    (lazy (player_army (~$ init_board_state) (~$ player_a))) 0 int;

  (* exceptions *)
  except_comp "board state invalid node"
    (lazy (node_owner (~$ init_board_state) "foo")) (UnknownNode "foo");
  except_comp "board state invalid cont"
    (lazy (cont_owner (~$ init_board_state) "foo")) (UnknownCont "foo");
  except_comp "board state invalid player"
    (lazy (player_nodes (~$ init_board_state) (~$ false_player)))
    (UnknownPlayer (~$ false_player));

  (* armies *)

  gen_comp "board state set armies jam"
    (lazy (node_army (~$ set_armies_state) "JAM")) 2 int;
  gen_comp "board state set armies lr7"
    (lazy (node_army (~$ set_armies_state) "LR7")) 0 int;

  gen_comp "board state add armies jam"
    (lazy (node_army (~$ add_armies_state) "JAM")) 4 int;
  gen_comp "board state add armies rpcc"
    (lazy (node_army (~$ add_armies_state) "RPCC")) 5 int;
  gen_comp "board state add armies lr7"
    (lazy (node_army (~$ add_armies_state) "LR7")) 0 int;

  gen_comp "board state player armies"
    (lazy (player_army (~$ player_a_own_rpcc_jam) (~$ player_a))) 9 int;
]

let init_game_state = lazy (Game_state.init (~$ map_schema) (~$ demo_players))

let player_a_own_RPCC_LR7 = lazy (set_owner (set_owner (~$ init_board_state) "RPCC" (Some (~$ player_a))) "LR7" (Some (~$ player_a)))

let attack_0_armies = lazy (change_board_st (turn_to_attack (~$ init_game_state)) (~$ player_a_own_RPCC_LR7))

let attack_state = lazy (change_board_st (~$ attack_0_armies) (set_army (~$ player_a_own_RPCC_LR7) "RPCC" 2))

let player_a_set_armies = lazy (change_board_st (~$ init_game_state) (set_army (~$ player_a_own_RPCC_LR7) "RPCC" 3))

let player_a_reinforce = lazy (reinforce (~$ player_a_set_armies) "LR7")

let attack_rpcc_hr5 = (lazy (attack (~$ attack_state) "RPCC" "HR5" 1))

(*let () = print_endline (string_of_int (player_army (~$ init_board_state) (~$ player_a)))
  let () = print_endline (pp_list str (player_nodes (~$ player_a_own_RPCC) (~$ player_a)))
  let () = print_endline ((opt player_p) (node_owner (~$ player_a_own_RPCC) "RPCC"))*)

let game_state_tests = [
  (* initial game state *)
  gen_comp "game state board state"
    (lazy (board_st (~$ init_game_state))) (~$ init_board_state) null;
  gen_comp "game state players"
    (lazy (players (~$ init_game_state))) (~$ demo_players) (pp_list player_p);
  gen_comp "game state current_player"
    (lazy (current_player (~$ init_game_state))) (~$ player_a) player_p;
  gen_comp "game state turn"
    (lazy (turn (~$ init_game_state))) Reinforce null;

  (* exceptions *)
  except_comp "game state no players" 
    (lazy (Game_state.init (~$ map_schema) [])) NoPlayers;
  except_comp "game state attack nonadjacent node"
    (lazy (attack (~$ attack_state) "RPCC" "LR7" 2)) 
    (FriendlyFire (Some (~$ player_a)));
  except_comp "game state invalid state" 
    (lazy (attack (~$ init_game_state) "LR7" "JAM" 2)) (InvalidState Reinforce);
  except_comp "game state insufficient armies"
    (lazy (attack (~$ attack_state) "RPCC" "HR5" 16)) 
    (InsufficientArmies ("RPCC", 1));
  except_comp "game state cannot attack oneself"
    (lazy (attack (~$ attack_state) "Keeton" "Rose" 2)) 
    (NotOwner "Keeton");

  (* reinforce *)
  gen_comp "game state reinforce"
    (lazy (node_army ((~$ player_a_reinforce) |> board_st) "LR7")) 1 null;

  (* attack *)
  gen_comp "game state attack"
    (lazy (node_army (let st', _, _ = (~$ attack_rpcc_hr5)
                      in st' |> board_st) "HR5")) 1 null;

]

let suite =
  "test suite for A678" >::: List.flatten [
    board_tests;
    player_tests;
    board_state_tests;
    game_state_tests;
  ]

let () = run_test_tt_main suite

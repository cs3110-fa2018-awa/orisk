open OUnit2
open Board

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
let str s = s
let bool = string_of_bool
let int = string_of_int
let coord (x, y) = "{x: " ^ (int x) ^ "; y: " ^ (int y) ^ "}"

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

let board_tests = [
  gen_comp "board name" (lazy (board_name (~$ map_schema))) "Cornell" str;
  gen_comp "board ascii" (lazy (board_ascii (~$ map_schema))) "" str;
  gen_comp "nodes" (lazy (nodes (~$ map_schema)))
    (List.sort compare ["RPCC"; "JAM"; "LR7"; "HR5"; "Keeton"; "Rose"]) (pp_list str);
  gen_comp "has node" (lazy (has_node (~$ map_schema) "RPCC")) true bool;
  gen_comp "doesn't have node" (lazy (has_node (~$ map_schema) "foo")) false bool;
  gen_comp "node name" (lazy (node_name (~$ map_schema) "JAM")) "Just About Music" str;
  except_comp "invalid node" (lazy (node_name (~$ map_schema) "foo")) (UnknownNode "foo");
  gen_comp "node coords" (lazy (node_coords (~$ map_schema) "RPCC")) (4, 5) coord;
  gen_comp "node borders" (lazy (node_borders (~$ map_schema) "JAM"))
    (List.sort compare ["LR7"; "RPCC"]) (pp_list str);
  gen_comp "conts" (lazy (conts (~$ map_schema)))
    (List.sort compare ["North"; "West"]) (pp_list str);
  gen_comp "has cont" (lazy (has_cont (~$ map_schema) "North")) true bool;
  gen_comp "doesn't have cont" (lazy (has_cont (~$ map_schema) "foo")) false bool;
  gen_comp "cont name" (lazy (cont_name (~$ map_schema) "North")) "North Campus" str;
  except_comp "invalid cont" (lazy (cont_name (~$ map_schema) "foo")) (UnknownCont "foo");
  gen_comp "cont nodes" (lazy (List.sort compare (cont_nodes (~$ map_schema) "North")))
    (List.sort compare ["RPCC"; "JAM"; "LR7"]) (pp_list str);
]

let suite =
  "test suite for A678" >::: List.flatten [
    board_tests;
  ]

let () = run_test_tt_main suite

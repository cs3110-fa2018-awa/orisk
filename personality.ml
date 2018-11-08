(** *) (* blank doc-comment to prevent docs from confusing first one *)

(** The abstract type representing the personality of a player. Personality
    can vary based on relative weights of [agression], [gambling], 
    [defensivity], [spite], and [spontaneity]. For example, a player with 
    a lot of [spite] will focus on breaking their opponents' continents. *)
type t = {
  aggression : int;
  gambling : int;
  defensivity : int;
  spite : int;
  spontaneity : int;
}

(** [max] is the maximum weight of any field *)
let max = 10

(** [personality_create a g d s spon] is a [personality] with [agression] set
    to [a], [gambling] set to [g], [defensivity] set to [d], [spite] set 
    to [s], and [spontaneity] set to [spon].

    Requires: [a], [g], [d], [s], and [spon] are all less than or equal to 
    [max], i.e. 10. *)
let personality_create a g d s spon = 
  {
    aggression = a;
    gambling = g;
    defensivity = d;
    spite = s;
    spontaneity = spon;
  }

(** [default] is the default [personality]. All fields are weighted equally. *)
let default = personality_create 5 5 5 5 5

(** [~~ a] is equivalent to applying [Pervasives.float_of_int] to [a]. *)
let (~~) a = float_of_int a

(** [node_heuristic p num] is the heuristic score of [num] nodes from 
    personality [p]. Positively weights conquering nodes. *)
let node_heuristic p num = (~~ num) *. 20. *. (~~ (p.aggression))

(** [bonus_heuristic p num] is the heuristic score of [num] bonus armies
    from personality [p]. Positively weights acquiring continents and 
    subsequently bonus armies. *)
let bonus_heuristic p num = (~~ num) *. 10. *. (~~ (p.aggression))

(** [army_heuristic p num] is the heuristic score of [num] armies from 
    personality [p]. Positively weights greater total armies. *)
let army_heuristic p num = (~~ num) *. 5. *. (~~ (p.aggression))

(** [region_heuristic p num] is the heuristic score of [num]
    distinct contiguous regions, i.e. paths of nodes owned by the same player,
    from personality [p]. 
    Negatively weights scattering nodes such that fortifying is difficult. *)
let region_heuristic p num = ~-. ((~~ num) *. 50.) *. (~~ (max - p.gambling))

(** [frontier_heuristic p num] is the heuristic score of [num] frontier nodes
    from personality [p]. Frontier node is defined as a node owned by a player
    that borders at least one opponent node. Negatively weights leaving
    more nodes vulnerable to enemy attack. *)
let frontier_heuristic p num = ~-. ((~~ num) *. 5.) *. (~~ (p.defensivity))

(** [frontier_armies_heuristic p num] is the heuristic score of [num]
    armies on frontier nodes from personality [p]. Frontier node is 
    defined the same as in [frontier_heuristic]. Positively weights placing
    armies on frontier nodes. *)
let frontier_armies_heuristic p num = ((~~ num) *. 5.) *. (~~ (p.defensivity))

(** [avg_frontier_armies_heuristic p num] is the heuristic score of [num] 
    average frontier armies from personality [p]. Frontier node is defined
    the same as in [frontier_heuristic]. Positively weights evenly
    spreading out armies among frontier nodes. *)
let avg_frontier_armies_heuristic p num = (num *. 15.) *. (~~ (p.defensivity))

(** [non_frontier_armies_heuristic p num] is the heuristic score of [num] 
    non-frontier armies from personality [p]. Frontier node is defined
    the same as in [frontier_heuristic]. Negatively weights placing armies
    on non-frontier nodes. *)
let non_frontier_armies_heuristic p num = ~-. (~~ num) *. 30.

(** [frontier_differential_heuristic p num] is the heuristic score of [num]
    frontier differential from personality [p]. Frontier node is defined
    the same as in [frontier_heuristic]. Negatively weights not
    putting armies on frontier nodes bordering opponent nodes with large 
    amount of armies. This is because doing so eaves the frontier node 
    vulnerable to easy opponent aquisition. *)
let frontier_differential_heuristic p num = 
  ~-. ((~~ num) *. 0.4) *. (~~ (p.defensivity))

(** [stars_heuristic p num] is the heuristic score of [num] stars from 
    personality [p]. Positively weights acquiring stars. *)
let stars_heuristic p num = (~~ num) *. 100.

(** [opponent_num_heuristic p num] is the heuristic score of [num] opponent
    players from personality [p]. Negatively weights more opponents so 
    incentivize opponent elimination. *)
let opponent_num_heuristic p num = ~-. ((~~ num) *. 5000.) *. (~~ (p.spite))

(** [max_opponent_heuristic p num] is the heuristic score of [num] where
    [num] is the maximum opponent heuristic from personality [p]. 
    Negatively weights allowing an opponent to dominate. *)
let max_opponent_heuristic p num = ~-. (num *. 0.1) *. (~~ (p.spite))

(** [avg_opponent_heuristic p num] is the heuristic score of [num] where
    [num] is the average opponent heuristic from personality [p]. 
    Negatively weights allowing the average opponent heuristic to increase. *)
let avg_opponent_heuristic p num = ~-. (num *. 0.05) *. (~~ (p.spite))
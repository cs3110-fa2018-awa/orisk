
type t = {
  aggression : int;
  gambling : int;
  defensivity : int;
  spite : int;
  spontaneity : int;
}

let default = {
  aggression = 5;
  gambling = 5;
  defensivity = 5;
  spite = 5;
  spontaneity = 5;
}

let max = 10

let (~~) a = float_of_int a

let node_heuristic p num = (~~ num) *. 20. *. (~~ (p.aggression))

let bonus_heuristic p num = (~~ num) *. 10. *. (~~ (p.aggression))

let army_heuristic p num = (~~ num) *. 5. *. (~~ (p.aggression))

let region_heuristic p num = ~-. ((~~ num) *. 50.) *. (~~ (max - p.gambling))

let frontier_heuristic p num = ~-. ((~~ num) *. 5.) *. (~~ (p.defensivity))

let frontier_armies_heuristic p num = ((~~ num) *. 5.) *. (~~ (p.defensivity))

let frontier_differential_heuristic p num = ~-. ((~~ num) *. 0.4) *. (~~ (p.defensivity))

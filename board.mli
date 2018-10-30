(** Representation of a game board. *)

(** The abtract type representing a board. *)
type t

(** The type of a coordinate pair (x,y). *)
type coords = (int * int)

(** The type of identifier for a node. *)
type node_id = string

(** The type of a continent. *)
type cont

(** The type of identifer for a continent. *)
type cont_id = string

(** The type of army size, an alias for int. *)
type army = int

(** [from_json json] is the board represented by [json]. *)
val from_json : Yojson.Basic.json -> t

(** [x coords] is the x coordinate of [coords]. *)
val x : coords -> int

(** [y coords] is the y coordinate of [coords]. *)
val y : coords -> int

(** [board_name board] is the name of [board]. *)
val board_name : t -> string

(** [board_ascii board] is the ASCII art string associated with [board]. *)
val board_ascii : t -> string

(** [board_ascii_height board] is the number of lines in the ASCII art
    associated with [board]. *)
val board_ascii_height : t -> int

(** [board_ascii_width board] is the number of characters in one line
    of the ASCII art associated with [board]. 

    Requires that the ASCII art is "rectangular". *)
val board_ascii_width : t -> int

(** [nodes board] is the list of all [node_id] in [board]. *)
val nodes : t -> node_id list

(** [fold_nodes board f acc] is a tail-recursive fold over all of the nodes
    in [board] with accumulator [acc]. [f] is a function that takes a node
    ID and an [acc] and produces the next [acc]. *)
val fold_nodes : t -> (node_id -> 'a -> 'a) -> 'a -> 'a

(** [has_node board node] is true iff [node] is a node of [board]. *)
val has_node : t -> node_id -> bool

(** [node_borders board node] is the list of nodes bordering [node]
    in [board].

    Raises [UnknownNode node] iff [has_node board node] is false. *)
val node_borders : t -> node_id -> node_id list

(** [node_name board node] is the name of [node] in [board].

    Raises [UnknownNode node] iff [has_node board node] is false. *)
val node_name : t -> node_id -> string

(** [node_coords board node] is the coordinates of [node] in [board].

    Raises [UnknownNode node] iff [has_node board node] is false. *)
val node_coords : t -> node_id -> coords

(** [conts board] is the list of all [cont_id] in [board]. *)
val conts : t -> cont_id list

(** [fold_conts board f acc] is a tail-recursive fold over all of the
    continents in [board] with accumulator [acc]. [f] is a function that
    takes a node ID and an [acc] and produces the next [acc]. *)
val fold_conts : t -> (cont_id -> 'a -> 'a) -> 'a -> 'a

(** [has_cont board cont] is true iff [cont] is a continent of [board]. *)
val has_cont : t -> cont_id -> bool

(** [cont_nodes board cont] is the list of all nodes in [cont] of [board].

    Raises [UnknownCont cont] iff [has_cont board cont] is false. *)
val cont_nodes : t -> cont_id -> node_id list

(** [cont_name board cont] is the name of [cont] in [board].

    Raises [UnknownCont cont] iff [has_cont board cont] is false. *)
val cont_name : t -> cont_id -> string

(** [cont_bonus board cont] is the number of bonus armies provided
    to the player controlling the entirety of [cont] in [board].

    Raises [UnknownCont cont] iff [has_cont board cont] is false. *)
val cont_bonus : t -> cont_id -> army

(** [node_conts board node] is the list of continents containing [node]
    in [board].

    Raises [UnknownNode node] iff [has_node board node] is false. *)
val node_conts : t -> node_id -> cont_id list

val node_search : t -> string -> node_id option

val nodes_filter : t -> (node_id -> bool) -> node_id list

(** [UnknownNode node] is the exception raised when [node] is not found. *)
exception UnknownNode of node_id

(** [UnknownCont cont] is the exception raised when [cont] is not found. *)
exception UnknownCont of cont_id

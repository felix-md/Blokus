module PiecePart : sig type t = { mutable i : int; mutable j : int; } end
module Piece :
  sig
    val matrix_height : int
    val matrix_width : int
    type color = NoColor | Red | Green | Blue | Yellow
    type t 


    val get_color : t -> color
    val get_parts : t -> PiecePart.t array  
    val get_pos : t -> int * int
    val get_nb_block : t -> int

    val set_pos : t -> int * int -> unit
    val set_color : t -> color -> unit

    val copy : t  -> t
    val copy_with_pos : t -> int * int -> t
    val create : int -> int array array -> color -> int * int -> int -> t
    val contains : t -> int -> int -> bool
    val min_i : t -> int
    val min_j : t -> int
    val max_i : t -> int
    val max_j : t -> int
    val flip : t -> unit
    val rotate : t -> unit
    val move : t -> int -> int -> unit
    val to_string : t -> string
  end

(* play.ml *)

module Play = struct

  type interface_interaction = 
    | Move of int * int
    | Rotate
    | Flip
    | Place
    | Cancel
    | Pass
    | None

    type placement = {
      piece : int;
      pos : int * int;
      rotation : int;
      flipped : bool;
    }
    
    type play = placement option
    
end

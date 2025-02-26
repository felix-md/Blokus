(* player.mli *)

open Piece

open Play
module Player : sig
  type t
  type game_state_token
  type interface_interaction = Play.interface_interaction
  type play = Play.play

  (* Constructor *)
  val create : ?test:bool -> ?human:bool -> string -> Piece.color -> t

  (* Getters *)
  val get_name : t -> string
  val get_color : t -> Piece.color
  val get_is_human : t -> bool
  val get_score : t -> int
  val get_pieces : t -> Piece.t array
  val get_in_hand_piece : t -> Piece.t option
  val get_choosing_index : t -> int
  val get_last_placed_piece : t -> Piece.t option
  val get_is_blocked : t -> bool
  val get_next_play : t -> play

  (* Protected setters - require game_state_token *)
  val update_score : game_state_token -> t -> int -> unit
  val create_game_state_token : unit -> game_state_token

  (* Public setters - gameplay related *)
  val remove_piece : Piece.t -> t -> unit
  val add_piece : Piece.t -> t -> unit
  val choose_piece : t -> Piece.t
  val increment_index : t -> unit
  val decrement_index : t -> unit
  val update_next_play : t -> unit
  val update_next_play_pos : t -> int -> int -> unit
  val update_next_play_rot : t -> unit
  val update_next_play_flip : t -> unit
 

 (* Public setters for gameplay state *)

val set_in_hand_piece : t -> Piece.t option -> unit
val set_choosing_index : t -> int -> unit
val set_last_placed_piece : t -> Piece.t option -> unit
val set_is_blocked : t -> bool -> unit
val set_next_play : t -> play -> unit
end
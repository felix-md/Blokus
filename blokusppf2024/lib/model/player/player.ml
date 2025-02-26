(* player.ml *)

open Piece
open Utils

open Play  (* Import the Play module *)

module Player = struct

  type game_state_token = unit

  type interface_interaction = Play.interface_interaction

  type play = Play.play

  type t = {
    name : string;
    color : Piece.color;
    is_human : bool;  
    mutable score : int;
    mutable pieces : Piece.t array;
    
    mutable in_hand_piece : Piece.t option;
    mutable choosing_index : int;
    mutable last_placed_piece : Piece.t option;
    mutable is_blocked : bool;
    mutable next_play : play;
  }

  (** 
    [create_piece_array color] creates an array of pieces for a given [color].
    ...
  *)
  let create_piece_array ?(test=false) color =
    let path = if test then "../assets/pieces.json" else "./assets/pieces.json" in
    let pieces_data = Utils.load_pieces path in
    let create_piece coords i =
      let index = List.length coords in  
      let nb_block = index in
      let parts = Array.of_list (List.map (fun (i, j) -> [| i; j |]) coords) in
      Piece.create nb_block parts color (0, 0) i

    in
    Array.of_list (List.mapi (fun i coords -> create_piece coords i) pieces_data)


  (** 
    Creates a new player with the given name and color.
    ...
  *)
  let create ?(test=false) ?(human=true) n c  = 
    {
      name = n;
      color = c;
      is_human = human;
      score = 0;
      pieces = create_piece_array ~test c;
      in_hand_piece = None;
      choosing_index = 0;
      is_blocked = false;
      last_placed_piece = None;
      next_play = None;
    }

  

  (** 
    [remove_piece p player] removes the piece [p] from the list of pieces 
    available to the [player].
    ...
  *)
  let remove_piece p player =
    player.pieces <- Array.of_list (List.filter (fun piece -> piece <> p) (Array.to_list player.pieces));
    player.last_placed_piece <- Some p

  (** 
    [add_piece p player] adds the piece [p] to the player's list of pieces.
    ...
  *)
  let add_piece (p:Piece.t) player  =
    (Piece.set_pos p (0, 0));
    player.pieces <- Array.append player.pieces [| p |]

  (** 
    [choose_piece player] allows the given [player] to choose a piece 
    from their available pieces.
    ...
  *)
  let choose_piece player =
    Random.self_init ();
    let random_index = Random.int (Array.length player.pieces) in
    let p = player.pieces.(random_index) in      
    remove_piece p player;
    p

  (** 
    Increments the index of the piece in the bag of the given player.
    ...
  *)
  let increment_index player =
    player.choosing_index <- (player.choosing_index + 1) mod (Array.length player.pieces)

  (** 
    Decrements the index of the piece in the bag of the given player.
    ...
  *)
  let decrement_index player =
    player.choosing_index <- (player.choosing_index - 1) mod (Array.length player.pieces);
    if player.choosing_index < 0 then player.choosing_index <- (Array.length player.pieces) - 1

  let update_next_play player =
    let placement:Play.placement = {
      piece = (player.choosing_index);
      pos = (0, 0);
      rotation = 0;
      flipped = false;
    }
  in
    player.next_play <- Some placement

  let update_next_play_pos player x y  = 
    match player.next_play with
    |None -> ()
    |Some play -> player.next_play <- Some {play with pos = (x, y)} 

  let update_next_play_rot player =
    match player.next_play with
    | None -> ()
    | Some play -> player.next_play <- Some {play with rotation = (play.rotation + 1) mod 4}

  let update_next_play_flip player =
    match player.next_play with
    | None -> ()
    | Some play -> player.next_play <- Some {play with flipped = not play.flipped}

  

  
    
  let create_game_state_token () = ()
  (* Getters *)
  let get_name player = player.name
  let get_color player = player.color
  let get_is_human player = player.is_human
  let get_score player = player.score
  let get_pieces player = Array.copy player.pieces
  let get_in_hand_piece player = 
    match player.in_hand_piece with
    | Some p -> Some ( p)
    | None -> None
  let get_choosing_index player = player.choosing_index
  let get_last_placed_piece player = match player.last_placed_piece with
    | Some p -> Some ( p)
    | None -> None
  let get_is_blocked player = player.is_blocked
  let get_next_play player = player.next_play

  (* Protected setters - requiring token *)
  let update_score _token player new_score = 
    player.score <- new_score

  (* Public setters for gameplay *)
  
  let set_in_hand_piece player piece = match piece with
    | None -> player.in_hand_piece <- None
    | Some p -> if Array.exists (fun x -> x = p) player.pieces then
       player.in_hand_piece <- Some ( p)
    else ()
  let set_choosing_index player index = player.choosing_index <- index
  let set_last_placed_piece player piece  = match piece with
    | None -> player.last_placed_piece <- None
    | Some p -> player.last_placed_piece <- Some (Piece.copy p)
  
  
  let set_is_blocked player blocked = player.is_blocked <- blocked
  let set_next_play player play = player.next_play <- play



end

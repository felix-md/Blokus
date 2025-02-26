(* game_state.ml *)
open Board
(* game_state.ml *)
open Board
open Player
open Piece
open Play


module Game_state = struct 

  let token = Player.create_game_state_token ()

  
  type t = {
    board : Board.t;
    players : Player.t array;
    current_player : int;
    
  }


 let create board players = {
   board = board;
   players = players;
   current_player = 0;
 }


  (** [is_blocked player board] checks if the given [player] is blocked on the [board].
    A player is considered blocked if they cannot place any piece of his bag.
    
    @param player The player to check.
    @param board The current state of the game board.
    @return true if the player is blocked, false otherwise.
  *)
  let is_blocked (player:Player.t) (board:Board.t) = 

    let not_placeable (piece:Piece.t) = not (fst (Board.is_placeable board piece)) in 
        
    Array.for_all (fun (piece:Piece.t) -> not_placeable (Piece.copy piece)) (Player.get_pieces player)

  
  (** [is_end_game state] checks if the game has reached its end state.
    @param state The current state of the game.
    @return true if the game is over, false otherwise.
  *)
  let is_end_game state = 
    let colors = Array.map (fun (player:Player.t) -> (Player.get_color player)) state.players in
    let placed_pieces = state.board.placed_pieces in
    
    if (
      not ( Array.for_all 
      (fun col ->(List.exists (fun (piece:Piece.t) -> Piece.get_color piece = col) 
      placed_pieces)) 
      colors)) 
      then false
    else
    (Array.iter
    (fun (player:Player.t) ->
        if (Player.get_is_blocked player) then ()
        else
        if(is_blocked player state.board )
        then (Player.set_is_blocked player true);
        ) 
        state.players;

    Array.for_all (fun (player:Player.t) -> (Player.get_is_blocked player) || (Array.length ((Player.get_pieces player)) = 0)) state.players)

    
  (** 
    [calculate_scores state] calculates the scores for the current game state.
    
    @param state The current state of the game.
    @return A list of scores for each player.
  *)
  let calculate_scores state =
    Array.iter (fun (player: Player.t) ->
      (* Calculate total unplaced squares *)
      let unplaced_squares = Array.fold_left (fun acc (piece:Piece.t) ->
        acc + Piece.get_nb_block piece
      ) 0 (Player.get_pieces player) in
      (* Negative points for unplaced squares *)
      Player.update_score token player (- unplaced_squares);
      
      
      (* Bonus points if all pieces are placed *)
      if Array.length (Player.get_pieces player) = 0 then
        match (Player.get_last_placed_piece player) with
        | Some piece when Piece.get_nb_block piece = 1 ->
            (* Bonus of 20 points *)
            Player.update_score token player ((Player.get_score player) + 20)
        | _ ->
            (* Bonus of 15 points *)
            Player.update_score token player ((Player.get_score player) + 15)
    ) state.players



  let get_current_player state = state.players.(state.current_player)

  let get_board state = 
    (* deep copy of the matrix *)
    let board= Array.map (fun row -> Array.copy row) state.board.board in
    {state.board with board =  board}


  let get_players state = state.players


  let increment_current_player state = 
    let next_player = (state.current_player + 1) mod (Array.length state.players) in
    { state with current_player = next_player }


  let find_winners state = 
    Array.fold_left (fun (acc:Player.t list) (player:Player.t) -> 

      if (Player.get_score player) > (Player.get_score (List.hd (acc))) then [player] 
      else if ((Player.get_score player) = (Player.get_score (List.hd (acc))) && player <>(List.hd (acc))) then player::acc
       else  acc
      ) [state.players.(0)] state.players


  let place_piece state (play:Play.placement) = 
    let piece = (Player.get_pieces (state.players.(state.current_player))).(play.piece) in

    Board.place_piece state.board piece
  
    



end
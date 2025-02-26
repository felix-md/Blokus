open Board
open Player
open Piece
open Game_state
open Play

module Game = struct

  (** 
    The type [game_state] is an alias for [Game_state.t], representing the state of the game.
  *)
  type game_state = Game_state.t

  type player = Player.t

  type interface_interaction = Player.interface_interaction

  type play = Play.play

 

  type error = 
    | InvalidMove
    | InvalidPlacement
    | AllPlayersBlocked

  type outcome = 
    | Next of game_state 
    | Error of error 
    | Endgame of player list 

  (** [init_game board_size player_names player_colors] initializes a new game with the given board size, player names, and player colors.
    @param board_size the size of the game board
    @param player_names a list of names of the players
    @param player_colors a list of colors assigned to the players
    @param player_types a list of booleans indicating whether the player is human or AI
    @return a new game_state instance initialized with the specified parameters
  *)
  let init_game board_size player_names player_colors player_types =
    let board = Board.create board_size in
    let players = Array.mapi (fun i name -> 
      Player.create ~human:player_types.(i) name player_colors.(i)) player_names 
    in
    Game_state.create board players
    

  (** [next_player state] returns the game state after transitioning to the next player.
    @param state The current game state.
    @return the new game state with the next player as current_player.
  *)
  let next_player (state : game_state)  = 
    let next_player = ref ( (Game_state.increment_current_player state)) in


      while Player.get_is_blocked (Game_state.get_current_player !next_player)do
        next_player := ((Game_state.increment_current_player !next_player));

     
      done;
      !next_player
      
    


  (** [handle_end_game state] handles the end of the game given the current [state].
    It performs necessary operations to conclude the game, such as determining
    the winner, updating scores, and any other end-of-game procedures.

    @param state The current state of the game.
    @return The updated state after handling the end of the game.
  *)
  let handle_end_game state = 
    if Game_state.is_end_game state then
      (let winner = 
        Game_state.find_winners state in
        Endgame winner)
    else
    
    Next (next_player state)


  (** [handle_play state play] processes a player's move in the game.

    @param state The current state of the game.
    @param play The move made by the player.
    @return The outcome of the move, which could include updates to the game state or results of the move.
  *)
  (* game.ml *)
  let handle_play (state : game_state) (play : play) : outcome = 
    let player = Game_state.get_current_player state in
    match play with
    | None -> 
      if Player.get_is_human player then
        (Player.set_is_blocked player true;
        Error InvalidMove)
      else
      Next (next_player state)

      
    | Some play -> 
      if play.piece < 0 || play.piece >= Array.length (Player.get_pieces player) then
        (Player.set_is_blocked player true;
        Next (next_player state))
      else  

      let piece = (Player.get_pieces player).(play.piece) in
      if not (Player.get_is_human player) then
        (for _ = 0 to play.rotation do Piece.rotate piece done;
        if play.flipped then Piece.flip piece;
        Piece.set_pos piece play.pos);
        
        if Game_state.place_piece state play then(
          let  index = play.piece in
          Player.remove_piece (Player.get_pieces player).(index) player;
          (Player.set_in_hand_piece player  None);
          handle_end_game state
        ) else
          if Player.get_is_human player then
            Error InvalidPlacement
          else
            (Player.set_is_blocked player true;
            Next (next_player state))
          

  

  
  (** [handle_interface_interaction state play] processes an interaction from the user interface 
    and updates the game state accordingly.

    @param state The current state of the game.
    @param play The interaction from the user interface.
    @return The outcome of processing the interaction.
  *)
  let handle_interface_interaction (state : game_state) (play : interface_interaction) : outcome =
    let player:Player.t = Game_state.get_current_player state in
    let current_piece = (Player.get_in_hand_piece player) in
    
    match current_piece with
    | None -> Error InvalidMove
    | Some piece ->
    match play with
    | Move (dx, dy) -> 
     
        Piece.move piece dx dy;
        Player.update_next_play_pos player dx dy;
        Next state
    | Rotate -> 

        Piece.rotate piece;
        Player.update_next_play_rot player;
        Next state
    | Flip -> 

        Piece.flip piece;
        Player.update_next_play_flip player;
        Next state
    | Place ->

      (match (Player.get_next_play player) with
      |None -> Error InvalidMove
      |Some play ->

      handle_play state (Some play))
    | Cancel -> 
        
        (Player.set_in_hand_piece player None);

        Next state
    | Pass ->
        
        (Player.set_in_hand_piece player None );
        handle_end_game state
    | None -> Next state








end

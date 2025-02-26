module Game :
  sig
    type game_state = Game_state.Game_state.t
    type player = Player.Player.t
    type interface_interaction = Play.Play.interface_interaction
    type play = Play.Play.play
    type error = InvalidMove | InvalidPlacement | AllPlayersBlocked
    type outcome =
        Next of game_state
      | Error of error
      | Endgame of player list


    val init_game :
      int ->
      string array -> Piece.Piece.color array -> bool array -> Game_state.Game_state.t

      
    val next_player : game_state -> Game_state.Game_state.t
    val handle_end_game : game_state -> outcome
    val handle_play : game_state -> play -> outcome
    val handle_interface_interaction :
      game_state -> interface_interaction -> outcome
  end

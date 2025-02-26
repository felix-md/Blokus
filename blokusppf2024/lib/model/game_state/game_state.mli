open Play
module Game_state :
  sig
    val token : Player.Player.game_state_token
    type t 
    val create : Board.Board.t -> Player.Player.t array -> t
    val is_blocked : Player.Player.t -> Board.Board.t -> bool
    val is_end_game : t -> bool
    val calculate_scores : t -> unit


    val get_current_player : t -> Player.Player.t
    val get_board : t -> Board.Board.t
    val get_players : t -> Player.Player.t array
    val increment_current_player : t -> t   


    val find_winners : t -> Player.Player.t list 
    
    val place_piece : t -> Play.placement -> bool   
  end

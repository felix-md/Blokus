open Game_state
open Player

open Play

module AI = struct
  
  (** [ai_move game_state player] determines the next move for the AI player.

    @param game_state The current state of the game.
    @param player The AI player for whom the move is being determined.
    @return The next move for the AI player as a [Play.play] option.
  *)
  let ai_move (gs : Game_state.t) (player : Player.t) : Play.play =
    (* Cette fonction doit renvoyer un type Play.play représentant le prochain coup de votre IA. 
      Via le Game_state on peut accéder à une copie du Board qui propose une fonction "is_placeable" 
      qui permet de vérifier si un placement est possible, ça devrais vous aider
      Attention: is_placeable ne prend pas en compte les règles du premier tour, vous devez les gérer vous même
    *)
    let _ = gs in
    let _ = player in

    None
    
end

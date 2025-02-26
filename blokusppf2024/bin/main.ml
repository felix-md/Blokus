(* bin/main.ml *)
module TermView = View.Terminal_view.TerminalView

open AI
open Piece
open Player
open Game
open Game_state
open Game_controller  

exception ExitLoop

let () =
  let player_names = [| "Player 1"; "Player 2"; "Player 3"; "Player 4" |] in
  let player_colors = [| Piece.Blue; Piece.Red; Piece.Green; Piece.Yellow |] in

  (* To activate IA mode for players *)
  let player_types = [| true; true; true; true |] in  (* true for human, false for AI *)
  let game_state = ref (Game.init_game 20 player_names player_colors player_types) in
  let original_termio = Unix.tcgetattr Unix.stdin in
  Game_controller.set_non_canonical_mode ();
  
  let process_play_outcome (outcome: Game.outcome) =
    match outcome with
    | Next state ->
        game_state := state
    | Error _err ->
        print_endline "Invalid move"
    | Endgame winner ->
        (* Handle end game *)
        Game_state.calculate_scores !game_state;
        Array.iter (fun (player:Player.t) ->
          Printf.printf "%s's score: %d\n" (Player.get_name player) (Player.get_score player)
        ) (Game_state.get_players !game_state);
        print_endline (
          String.concat " " (List.map (fun (player:Player.t) -> Player.get_name player) winner) ^ " wins!"
        );
        raise ExitLoop
  in
  try
    
    while true do
      Game_state.calculate_scores !game_state;
      Game_controller.clear_screen ();
      let current_player = ref (Game_state.get_current_player !game_state) in


      
      (* Piece selection loop *)
      if Player.get_is_human !current_player then

      begin
        while (Player.get_in_hand_piece !current_player) = None do
          Game_controller.clear_screen ();
          TermView.print_piece_bag !game_state;
          match Game_controller.handle_piece_selection_input !current_player with
          | `Continue -> ()
          | `PieceSelected -> ()
          | `Pass ->
              game_state := Game.next_player !game_state;
              current_player := Game_state.get_current_player !game_state
          | _ -> ()
        done;

        
        (* Game lopp *)
        Game_controller.clear_screen ();
        TermView.print_flashing_piece !game_state 1 1;
        let interface_interaction_option = Game_controller.handle_game_input () in

        match interface_interaction_option with
        | Some play ->
            begin
              match play with
              | Place ->
                (* User want to place the piece *)
                let play_option = Player.get_next_play !current_player in
                let outcome = Game.handle_play !game_state play_option in
                process_play_outcome outcome

              | _ ->
                  (* update current play *)
                  ignore (Game.handle_interface_interaction !game_state play)
            end
        | None -> ()
      end
    else 
      begin
        (* AI player's turn *)
        TermView.print_board (Game_state.get_board !game_state) 1 1;
        let ai_play = AI.ai_move !game_state !current_player in
      
        let outcome = Game.handle_play !game_state ai_play in
        process_play_outcome outcome


      end
    done


  with
  | ExitLoop ->
    Game_controller.restore_canonical_mode original_termio;
      print_endline "Exiting..."

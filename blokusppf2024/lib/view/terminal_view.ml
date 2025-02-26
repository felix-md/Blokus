open Player
open Board
open Piece
open Game_state


module TerminalView = struct



  type game_state = Game_state.t

  (** 
    Converts a given color to its corresponding ANSI color code.

    @param color The color to be converted.
    @return The ANSI color code as a string.
  *)
  let to_ANSI_color color = 
    match color with
      | Piece.Red -> ANSITerminal.red
      | Piece.Green -> ANSITerminal.green
      | Piece.Blue -> ANSITerminal.blue
      | Piece.Yellow -> ANSITerminal.yellow
      | Piece.NoColor-> ANSITerminal.white

  

  (** [print_board board] prints the current state of the board to the terminal.
    @param board The board to be printed, of type [Board.t].
  *)
  let print_board (board : Board.t) x y  =
    for i = 0 to board.size - 1 do
      ANSITerminal.set_cursor x (y+i);
      for j = 0 to board.size -1 do
        let value = Board.get board i j in
        match value with
        | None -> ANSITerminal.print_string [ANSITerminal.black] "██"
        | Some v ->  ANSITerminal.print_string [to_ANSI_color v] "██"
                    
      done;
      print_endline "";
    done
   

  (** [print_flashing_piece board piece] prints a flashing representation of the given [piece] on the [board].
  
  @param board The current state of the game board.
  @param piece The piece to be displayed in a flashing manner.
  *)
  let print_flashing_piece  (gs : game_state) x y  =
    let player = Game_state.get_current_player gs  in
    let board = Game_state.get_board gs in
    let piece = (Player.get_in_hand_piece player) in
    match piece with
    | None -> ()
    | Some piece ->
    for i = 0 to board.size - 1 do
      ANSITerminal.set_cursor x (y+i);
      for j = 0 to board.size - 1  do
        if Piece.contains piece i j then
          ANSITerminal.print_string [to_ANSI_color (Piece.get_color piece); ANSITerminal.Blink] "██"
        else
          let value = Board.get board i j in
          match value with
          | None -> ANSITerminal.print_string [ANSITerminal.black] "██"
          | Some v ->  ANSITerminal.print_string [to_ANSI_color v] "██"
          
          
        done;
        
      done;
      ANSITerminal.set_cursor  45 1;
      ANSITerminal.print_string [ANSITerminal.white] "Controls: ";
      ANSITerminal.set_cursor  45 3;
      ANSITerminal.print_string [ANSITerminal.white] "Press '↑' to move up, '↓' to move down,";
      ANSITerminal.set_cursor  45 5;
      ANSITerminal.print_string [ANSITerminal.white]"'←' to move left, '→' to move right";
      ANSITerminal.set_cursor  45 7;
      ANSITerminal.print_string [ANSITerminal.white]"'r' to rotate, 'f' to flip";
      ANSITerminal.set_cursor  45 9;
      ANSITerminal.print_string [ANSITerminal.white]"'space' to place, 'c' to cancel";
      ANSITerminal.set_cursor  45 11;
      ANSITerminal.print_string [ANSITerminal.white]"'p' to pass, 'q' to quit";
      
      ANSITerminal.set_cursor 45 19;
    ANSITerminal.print_string [ANSITerminal.white] 
    ("Player: " ^ (Player.get_name player) );

    ANSITerminal.set_cursor 1 25;

    flush stdout
    

    

  (** [print_piece piece x y] prints the given piece at the specified coordinates (x, y) in the terminal view.
    @param piece The piece to be printed.
    @param x The x-coordinate where the piece will be printed.
    @param y The y-coordinate where the piece will be printed.
  *)
  let print_piece (piece:Piece.t) x y = 
    for i = 0 to Piece.matrix_height - 1 do
      ANSITerminal.set_cursor x (y+i);
      for j = 0 to Piece.matrix_width - 1 do
        if Piece.contains piece i j then
          ANSITerminal.print_string [to_ANSI_color (Piece.get_color piece)] "██"
        else
          ANSITerminal.print_string [ANSITerminal.Hidden] "██"
      done;
      print_endline "";
    done

  (** [print_piece_bag gs] prints the pieces available in the game state [gs].
    @param gs The current game state containing the pieces to be printed.
  *)
  let print_piece_bag (gs:game_state) = 
    let player = Game_state.get_current_player gs in
    let board = Game_state.get_board gs in
    print_board board 1 1;
    
    let piece = (Player.get_pieces player).((Player.get_choosing_index player)) in

    ANSITerminal.set_cursor  45 1;
    ANSITerminal.print_string [ANSITerminal.white] "Piece Bag : ";
    ANSITerminal.set_cursor 45 3;
    ANSITerminal.print_string [ANSITerminal.white] "Press '←' to move left, '→' to move right, 'space' to select";
    ANSITerminal.set_cursor 45 5;
    ANSITerminal.print_string [ANSITerminal.white] "Press 'p' to pass, 'q' to quit";


    print_piece piece 45 7;
    ANSITerminal.set_cursor 45 15;
    ANSITerminal.print_string [ANSITerminal.white]
     ("Piece: " ^ string_of_int ((Player.get_choosing_index player) + 1 ) ^ "/" ^ string_of_int (Array.length (Player.get_pieces player)));

    ANSITerminal.set_cursor 45 17;
    ANSITerminal.print_string [ANSITerminal.white] 
    ("Score: " ^ string_of_int (Player.get_score player));


    ANSITerminal.set_cursor 45 19;
    ANSITerminal.print_string [ANSITerminal.white] 
    ("Player: " ^ (Player.get_name player) );

    ANSITerminal.set_cursor 1 25;

    flush stdout





  


    
end
  
  
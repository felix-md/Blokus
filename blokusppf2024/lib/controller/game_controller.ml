(* game_controller.ml *)
open Unix

open Player
open Play

exception ExitLoop
module Game_controller = 
struct 
  (** 
    Sets the terminal to non-canonical mode.

    This function modifies the terminal settings to disable canonical mode,
    which allows input to be read character-by-character without waiting for
    a newline. It also sets the minimum number of characters for non-canonical
    read to 1 and the timeout to 0.

    @raise Unix.Unix_error if there is an error in getting or setting the terminal attributes.
  *)
  let set_non_canonical_mode () =
    let termio = tcgetattr stdin in
    let new_termio = { termio with c_icanon = false; c_vmin = 1; c_vtime = 0 } in
    tcsetattr stdin TCSANOW new_termio

  (** [restore_canonical_mode termio] restores the terminal to its canonical mode using the provided terminal attributes [termio].
    This function uses [tcsetattr] to set the terminal attributes immediately (TCSANOW) for the standard input (stdin).

    @param termio The terminal attributes to be restored.
  *)
  let restore_canonical_mode termio =
    tcsetattr stdin TCSANOW termio

  (** [read_single_char ()] reads a single character from the standard input.
    It creates a buffer of size 1, reads one byte from the standard input into the buffer,
    and returns the character at the first position of the buffer.
    
    @return the character read from the standard input.
  *)
  let read_single_char () =
    let buf = Bytes.create 1 in
    ignore (Unix.read Unix.stdin buf 0 1);
    Bytes.get buf 0

  (** [clear_screen ()] clears the terminal screen by printing the appropriate
    escape sequences to move the cursor to the home position and clear the screen.
    It then flushes the standard output to ensure the changes are displayed immediately. *)
  let clear_screen () =
    print_string "\027[2J";
    print_string "\027[H";
    flush Stdlib.stdout



  (** 
    Handles the input for piece selection during the game.

    @param current_player The player whose input is being handled.

    The function reads a single character input from the user and performs actions based on the input:
    - If the input is the escape character ('\027'), it reads additional characters to determine if an arrow key was pressed:
      - 'C' (right arrow) increments the player's piece selection index.
      - 'D' (left arrow) decrements the player's piece selection index.
    - If the input is a space character (' '), it selects the piece at the current index, removes it from the player's available pieces, and resets the selection index.
    - If the input is 'p', it indicates that the player wants to pass their turn.
    - If the input is 'q', it raises an exception to exit the input loop.
    - For any other input, it continues without making changes.

    @return A variant indicating the result of the input handling:
    - `Continue` if the input handling should continue.
    - `PieceSelected` if a piece was selected.
    - `Pass` if the player chose to pass their turn.
    - Raises `ExitLoop` if the player chose to quit.
  *)
  let handle_piece_selection_input current_player =

   
    let user_input = read_single_char () in
    match user_input with
    | '\027' -> 
      let next_char = read_single_char () in
      if next_char = '[' then
        let arrow_key = read_single_char () in
        match arrow_key with
        | 'C' -> Player.increment_index current_player; `Continue  
        | 'D' -> Player.decrement_index current_player; `Continue  
        | _ -> `Continue
      else `Continue
    | ' ' ->
        Player.set_in_hand_piece current_player (Some (Player.get_pieces current_player).((Player.get_choosing_index current_player)));
        Player.update_next_play current_player;
        
        Player.set_next_play current_player (Some {piece = Player.get_choosing_index current_player; rotation = 0; flipped = false; pos = (0, 0)});
        (* Player.remove_piece (Player.get_pieces current_player).((Player.get_choosing_index current_player)) current_player; *)
        (Player.set_choosing_index current_player  0);
        `PieceSelected
    | 'p' -> `Pass
    | 'q' -> raise ExitLoop
    | _ -> `Continue


  (** 
    Handles the game input from the user. This function reads a single character
    from the user input and matches it to perform corresponding game actions.

    - If the input is an escape character ('\027'), it reads the next character to check for arrow keys:
      - 'C' -> Moves the game piece right.
      - 'D' -> Moves the game piece left.
      - 'B' -> Moves the game piece down.
      - 'A' -> Moves the game piece up.
    - 'f' -> Flips the game piece.
    - 'r' -> Rotates the game piece.
    - ' ' (space) -> Places the game piece.
    - 'c' -> Cancels the current action.
    - 'p' -> Passes the turn.
    - 'q' -> Exits the game loop by raising an ExitLoop exception.
    - Any other input returns None.

    @return An option type with the corresponding game action or None if the input is not recognized.
  *)
  let handle_game_input () =
    let user_input = read_single_char () in
    match user_input with
    | '\027' -> 
      let next_char = read_single_char () in
      if next_char = '[' then
        let arrow_key = read_single_char () in
       
        match arrow_key with
        | 'C' -> Some (Play.Move (0, 1))  
        | 'D' -> Some (Play.Move (0, -1))   
        | 'B' -> Some (Play.Move (1, 0))       
        | 'A' -> Some (Play.Move (-1, 0))     
        | _ -> None
        
      else None
    | 'f' -> Some Play.Flip
    | 'r' -> Some Play.Rotate
    | ' ' -> Some Play.Place
    | 'c' -> Some Play.Cancel
    | 'p' -> Some Play.Pass
    | 'q' -> raise ExitLoop
    | _ -> None



end

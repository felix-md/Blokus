
open Piece



module Board = struct 

  type t = {
    size : int;
    mutable board : Piece.color array array;
    nb_players : int;
    mutable placed_pieces : Piece.t list;
  }

  (** 
    Creates a new board of the given size.

    @param size The size of the board to be created.
    @return A new board with the specified size.
  *)
  let create size = 
    if size <=0 then failwith "Invalid size";{
    size = size;
    board = Array.make_matrix size size (Piece.NoColor);
    nb_players = 4;
    placed_pieces = [];
  }
 

  (** 
    Checks if the given coordinates (x, y) are within the bounds of the matrix.

    @param m The matrix with a defined size.
    @param x The x-coordinate to check.
    @param y The y-coordinate to check.
    @return true if the coordinates are within the bounds of the matrix, false otherwise.
  *)
  let in_matrix m x y = x >= 0 && x < m.size && y >= 0 && y < m.size

  (** 
    Retrieves the value at the specified coordinates (x, y) from the board matrix.

    @param m The board matrix from which to retrieve the value.
    @param x The x-coordinate of the value to retrieve.
    @param y The y-coordinate of the value to retrieve.
    @return [Some value] if the coordinates are within the bounds of the matrix, 
            [None] otherwise.
  *)
  let get m x y = if not (in_matrix m x y) then None else Some (m.board.(x).(y))

  (** 
    Sets the value at the specified coordinates in the matrix if the coordinates are within bounds.

    @param matrix The matrix in which the value is to be set.
    @param x The x-coordinate of the position.
    @param y The y-coordinate of the position.
    @param value The value to set at the specified position.
    @return Unit. The function modifies the matrix in place.
  *)
  let set  matrix x y value =  if not (in_matrix matrix x y) then () else matrix.board.(x).(y) <- value

  (** 
    [find_forbidden_places m col] returns a list of positions on the board [m] 
    that are forbidden for the piece of color [col] to be placed. 

    @param m the current state of the board
    @param col the color of the piece
    @return a list of positions that are forbidden for the piece of color [col]
  *)
  let find_forbidden_places m (col:Piece.color) = 
    let forbidden_places = ref [] in
    let neighboors = [ (0, 1); (1, 0); (0, -1); (-1, 0) ] in
    let is_adjacent_colored i j =
      List.exists (fun (di, dj) -> 
        if in_matrix m (i+di) (j+dj) then 
          m.board.(i + di).(j + dj) = col
      else false) neighboors
    in
    for i = 0 to m.size - 1 do
      for j = 0 to m.size - 1 do
        if m.board.(i).(j) = Piece.NoColor then
        if is_adjacent_colored i j then
          forbidden_places := (i, j) :: !forbidden_places
      done
    done;
    !forbidden_places

  (** [find_obligatory_places m col] finds the obligatory places on the board [m] for the given piece color [col].
    @param m the board matrix
    @param col the color of the piece
    @return a list of positions that are obligatory for the given piece color, a piece of that color must be placed
    at one of these positions at least.
  *)
  let find_obligatoy_places m (col:Piece.color) = 
    let obligatoy_places = ref [] in
    let diag_neighboors = [ (1, 1); (1, -1); (-1, -1); (-1, 1) ] in
    let neighboors = [ (0, 1); (1, 0); (0, -1); (-1, 0) ] in
    let is_diag_colored i j =
      List.exists (fun (di, dj) -> 
        if in_matrix m (i+di) (j+dj) then 
          m.board.(i + di).(j + dj) = col
       else false) diag_neighboors
    in
    let is_adjacent_empty i j =
      List.for_all (fun (di, dj) -> 
        if in_matrix m (i+di) (j+dj) then 
          m.board.(i + di).(j + dj) != col
       else true) neighboors
    in
    for i = 0 to m.size - 1 do
      for j = 0 to m.size - 1 do
        if is_diag_colored i j  && is_adjacent_empty i j then
            obligatoy_places := (i, j) :: !obligatoy_places
      done
    done;
    !obligatoy_places
  

  
  (** [first_piece p] returns a boolean that indicate if the condtions are valid
    for a first piece.
    @param p The player whose first piece is to be returned.
    @return a boolean. *)
  let first_piece p =
    let corners = [ (0, 0); (0, 19); (19, 0); (19, 19) ] in
    List.exists (fun (i, j) -> Piece.contains p i j) corners

  (** [aux_place_piece m p] places the piece [p] on the board [m].
    It calculates the minimum and maximum coordinates of the piece
    based on its position and iterates over the range to set the
    piece's color on the board. The piece is then added to the list
    of placed pieces in the board [m].

    @param m the board on which the piece is to be placed
    @param p the piece to be placed on the board
    @return true after the piece is successfully placed
  *)
  let aux_place_piece m p = 
    let min_i = (Piece.min_i p) + fst  (Piece.get_pos p) in
    let min_j = (Piece.min_j p) + snd  (Piece.get_pos p) in
    let max_i = (Piece.max_i p) + fst  (Piece.get_pos p) in
    let max_j = (Piece.max_j p) + snd  (Piece.get_pos p) in
    for i = min_i to max_i do
      for j = min_j to max_j do
        if Piece.contains p i j then
          set m i j (Piece.get_color p)
      done
    done;
    m.placed_pieces <- p :: m.placed_pieces;
    true

  (** [place_piece m p] places the piece [p] on the board [m].

    @param m The current state of the board.
    @param p The piece to be placed on the board.
    @return The updated state of the board after placing the piece.
  *)
  let place_piece ?(first=true) m (p:Piece.t) = 
    (* if one piecepart is not in the matrix *)
    if Array.exists 
      (fun piece_part -> 
      not (in_matrix m (piece_part.PiecePart.i + fst((Piece.get_pos p))) (piece_part.PiecePart.j + snd((Piece.get_pos p))))) (Piece.get_parts p) then
        false
    
      
    (* check if the piece is the first piece of this color *)
    else if not (List.exists (fun (piece:Piece.t) -> ((Piece.get_color piece)) = (Piece.get_color p)) m.placed_pieces) then
      (* check if the first piece condition are *)
      if ((first )&& first_piece p) then
        if Array.exists 
          (fun piece_part -> 
            m.board.(piece_part.PiecePart.i + fst((Piece.get_pos p))).(piece_part.PiecePart.j + snd((Piece.get_pos p))) != Piece.NoColor) (Piece.get_parts p) then
        false
          else 
        aux_place_piece m p
      else 
        false

    else if 
      (* Check if there is already another piece at this coordinates *)
      Array.exists 
        (fun piece_part -> 
          m.board.(piece_part.PiecePart.i + fst((Piece.get_pos p))).(piece_part.PiecePart.j + snd((Piece.get_pos p))) != Piece.NoColor) (Piece.get_parts p) then
      false
    else
      (* check the forbidden places *)
      let forbidden_places = find_forbidden_places m (Piece.get_color p) in
      (* check the obligatory places *)
      let obligatory_places = find_obligatoy_places m (Piece.get_color p) in

      (* if the piece will be on one of the forbidden places *)
      if List.exists (fun (i, j) -> Piece.contains p i j) forbidden_places then
        false

      (* if the piece will be on none of the obligatory places *)
      else if List.length obligatory_places > 0 && not (List.exists (fun (i, j) -> Piece.contains p i j) obligatory_places) then
        false
      else
        (* place the piece if the position is valid *)
        aux_place_piece m p


  (** [is_placeable m p] checks if the piece [p] can be placed on the board [m].
      
      @param m The current state of the board.
      @param p The piece to be placed on the board.
      @return two values: a boolean indicating if the piece can be placed, and the piece itself 
  *)

  let is_placeable m (p:Piece.t) =  
    let obligatory_places = find_obligatoy_places m (Piece.get_color p) in
    let find_forbidden_places = find_forbidden_places m (Piece.get_color p) in
    let aux m p = 
      let min_i = (Piece.min_i p) + fst  (Piece.get_pos p) in
      let min_j = (Piece.min_j p) + snd  (Piece.get_pos p) in
      let max_i = (Piece.max_i p) + fst  (Piece.get_pos p) in
      let max_j = (Piece.max_j p) + snd  (Piece.get_pos p) in
      let is_placeable = ref true in
      if not (List.exists (fun (i, j) -> Piece.contains p i j) obligatory_places) then
        is_placeable := false;
        
      if List.exists (fun (i, j) -> Piece.contains p i j) find_forbidden_places then
        is_placeable := false;
      
      
      if  !is_placeable then

        (for i = min_i to max_i do
          for j = min_j to max_j do
            if Piece.contains p i j then
              if not (in_matrix m i j) || m.board.(i).(j) != Piece.NoColor then
                is_placeable := false
          done
        done;
        !is_placeable)
      else 
      !is_placeable
    in
    let result = ref false in
    let piece = ref( Piece.copy p) in
    for i = 0 to m.size - 1 do
      for j = 0 to m.size - 1 do

        if !result then
          ()
        else


        let new_p = Piece.copy_with_pos p (i,j)  in

        if Array.for_all 
          (fun part -> in_matrix m (part.PiecePart.i + i) (part.PiecePart.j + j)) 
        (Piece.get_parts new_p) then
        (for _ = 0 to 3 do
          let flipped = Piece.copy new_p in
          Piece.flip flipped;
          if aux m new_p || aux m flipped then
            (result := true;
            piece := new_p;)
            
          else
            Piece.rotate new_p
          done)
      done
    done;
    !result, !piece



end
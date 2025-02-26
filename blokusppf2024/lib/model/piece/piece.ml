


module PiecePart = struct
  type t = {
    mutable i : int;
    mutable j : int;
  }
end

module Piece =
struct
  let matrix_height = 20
  and matrix_width = 20;

  type color = 
    | NoColor
    | Red
    | Green
    | Blue
    | Yellow


  type t = {
    nb_block : int;
    parts : PiecePart.t array;
    mutable color : color;
    mutable pos : int * int;
    id :int;
  }


  (** [get_color p] returns the color of the piece [p]. *)
  let get_color p = p.color

  (** [get_parts p] returns the parts of the piece [p]. *)
  let get_parts p = p.parts

  (** [get_pos p] returns the position of the piece [p]. *)
  let get_pos p = p.pos

  (** [get_nb_block p] returns the number of blocks of the piece [p]. *)
  let get_nb_block p = p.nb_block


  (** [set_pos p pos] sets the position of the piece [p] to [pos]. *)
  let set_pos p pos = p.pos <- pos

  let set_color p color = p.color <- color


  





  (** [copy p] returns a new piece that is a duplicate of [p], including its number of blocks, parts, color, and position. *)
  let copy p = {
    nb_block = p.nb_block;
    parts = Array.map (fun part -> { PiecePart.i = part.PiecePart.i; PiecePart.j = part.PiecePart.j }) p.parts;
    color = p.color;
    pos = p.pos;
    id = p.id;
  }
  
  let copy_with_pos p position = {
  nb_block = p.nb_block;
  parts = Array.copy p.parts;
  color = p.color;
  pos = position;
  id = p.id;
}



  (** 
    Creates a new piece for the game.

    @param nb_block The number of blocks that make up the piece.
    @param parts A 2D array representing the shape of the piece.
    @param color The color of the piece.
    @param pos The initial position of the piece on the board.
    @param id The unique identifier of the piece.

    @return A new piece with the specified attributes.
  *)
  let create nb_block (parts: int array array) (color:color) pos id= 
    let parts = Array.map (fun part -> {PiecePart.i = part.(0); PiecePart.j = part.(1)}) parts in
    {nb_block = nb_block; parts = parts; color = color; pos=pos ; id=id}


  (** 
    Checks if the piece [p] contains the cell at coordinates ([i], [j]).

    @param p The piece to check.
    @param i The row index of the cell.
    @param j The column index of the cell.
    @return [true] if the piece contains the cell at ([i], [j]), [false] otherwise.
  *)
  let contains p i j = 
    Array.exists (fun part -> part.PiecePart.i + fst(p.pos)  = i && part.PiecePart.j + snd(p.pos)= j) p.parts

  (** 
    [min_i p] returns the minimum index [i] of the piece [p].
    This function is used to determine the highest point of the piece.
    
    @param p The piece for which the minimum index is to be found.
    @return The minimum index [i] of the piece [p]..
  *)
  let min_i p = 
    Array.fold_left (fun acc part -> min acc part.PiecePart.i) p.parts.(0).PiecePart.i p.parts

  (** 
    [min_j p] returns the minimum index [j] of the piece [p].
    This function is used to determine point of the piece furthest to the left.
    
    @param p The piece for which the minimum index is to be found.
    @return The minimum index [j] of the piece [p]..
  *)
  let min_j p =
    Array.fold_left (fun acc part -> min acc part.PiecePart.j) p.parts.(0).PiecePart.j p.parts

  (** 
    [max_i p] returns the maximum index [i] of the piece [p].
    This function is used to determine the lowest point of the piece.
    
    @param p The piece for which the maximum index is to be found.
    @return The maximum index [i] of the piece [p].
  *)
  let max_i p =
    Array.fold_left (fun acc part -> max acc part.PiecePart.i) p.parts.(0).PiecePart.i p.parts

  (** 
    [max_j p] returns the maximum index [j] of the piece [p].
    This function is used to determine point of the piece furthest to the right.
    
    @param p The piece for which the maximum index is to be found.
    @return The maximum index [j] of the piece [p].
  *)
  let max_j p =
    Array.fold_left (fun acc part -> max acc part.PiecePart.j) p.parts.(0).PiecePart.j p.parts
  
  (** 
    Flips the piece [p] horizontally.

    This function flips the [p] horizontally by mirroring its parts along the vertical axis.
    It first determines the maximum column index (j) among the parts of the [p].
    Then, it calculates the new column index for each part by subtracting the current column index from the maximum column index.
    After flipping the parts, it checks if the new positions of the parts are within the board limits.
    If the new positions are valid, it updates the [p]'s parts with the new column indices.

    @param p The piece to be flipped.
  *)
  let flip p =
    (* Find the maximum j among the parts *)
    let max_j = Array.fold_left (fun acc part -> max acc part.PiecePart.j) p.parts.(0).PiecePart.j p.parts in
    (* Flip the parts *)
    let new_parts = Array.map (fun part ->
      let new_j = max_j - part.PiecePart.j in
      { part with j = new_j }
    ) p.parts in
    (* Check if the new parts are within the board limits *)
    let in_bounds = Array.for_all (fun part ->
      let i = part.PiecePart.i + fst p.pos in
      let j = part.PiecePart.j + snd p.pos in
      i >= 0 && i < matrix_height && j >= 0 && j < matrix_width
    ) new_parts in
    if in_bounds then
      (* Update the piece parts *)
      Array.iteri (fun idx _part ->
      p.parts.(idx).j <- new_parts.(idx).j
      ) p.parts

  (** 
    Rotates the given piece 90 degrees counterclockwise while maintaining its relative position on the board.

    @param p The piece to be rotated, which includes its parts and position.

    The function performs the following steps:
    1. Saves the original minimum coordinates of the [p] parts.
    2. Rotates each part of the [p] 90 degrees counterclockwise.
    3. Calculates the new minimum coordinates after rotation.
    4. Computes the offsets needed to align the new minimum coordinates with the original ones.
    5. Adjusts the rotated parts using the calculated offsets.
    6. Checks if the adjusted parts are within the board limits.
    7. If the adjusted parts are within bounds, updates the [p] parts with the new coordinates.

    @return [unit]
  *)
  let rotate p =
    (* Save the original minimum coordinates *)   
      let orig_min_i = min_i p in
      let orig_min_j = min_j p in
      (* Rotate the parts *)
      let new_parts = Array.map (fun part ->
        let i = part.PiecePart.i in
        let j = part.PiecePart.j in
        let new_i = j in
        let new_j = -i in
        (new_i, new_j)
      ) p.parts in
      (* Calculate the new minima *)
      let new_min_i = Array.fold_left (fun acc (i, _) -> min acc i) (fst new_parts.(0)) new_parts in
      let new_min_j = Array.fold_left (fun acc (_, j) -> min acc j) (snd new_parts.(0)) new_parts in
      (* Calculate the offsets to align with the original minima *)
      let di = orig_min_i - new_min_i in
      let dj = orig_min_j - new_min_j in
      (* Adjust the new parts *)
      let adjusted_parts = Array.map (fun (i, j) ->
        let adj_i = i + di in
        let adj_j = j + dj in
        (adj_i, adj_j)
      ) new_parts in
      (* Check if the adjusted parts are within the board limits *)
      let in_bounds = Array.for_all (fun (i, j) ->
        let i = i + fst p.pos in
        let j = j + snd p.pos in
        i >= 0 && i < matrix_height && j >= 0 && j < matrix_width
      ) adjusted_parts in
      if in_bounds then
        (* Update the piece parts *)
        Array.iteri (fun idx part ->
          let (i, j) = adjusted_parts.(idx) in
          part.PiecePart.i <- i;
          part.PiecePart.j <- j;
        ) p.parts

  (** 
    Moves the piece `p` by `i` units vertically and `j` units horizontally.

    @param p The piece to be moved.
    @param i The number of units to move the piece vertically.
    @param j The number of units to move the piece horizontally.

    if the piece is out of bounds after the move, the piece is not moved.

    @return Unit. The function modifies the piece in place.
  *)
  let move p i j = 
    let new_pos = (fst p.pos + i, snd p.pos + j) in
    let new_p = { p with pos = new_pos } in
    let min_i = min_i new_p + fst new_pos in
    let min_j = min_j new_p + snd new_pos in
    let max_i = max_i new_p + fst new_pos in
    let max_j = max_j new_p + snd new_pos in
    if (min_i >= 0) && (min_j >= 0) && (max_i < matrix_height) && (max_j < matrix_width) then
      p.pos <- new_pos

  let to_string p = 
    let str = ref "" in
    for i = 0 to matrix_height - 1 do
      for j = 0 to matrix_width - 1 do
        if contains p i j then
          str := !str ^ "X"
        else
          str := !str ^ "."
      done;
      str := !str ^ "\n"
    done;
    !str
   
end
  
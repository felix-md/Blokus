open OUnit2
open Piece
open Board



module P = Piece
module PP = PiecePart



let part_gen = QCheck.map (fun (x, y) -> [|x; y|]) QCheck.(pair (int_bound 19) (int_bound 19)) 
let make_random_piece_parts n = 
  Array.init n (fun _ -> QCheck.Gen.generate1 part_gen.gen)
let piece_gen =
  QCheck.make
    ~print:(fun (piece:Piece.t) ->
      let pos_x, pos_y = Piece.get_pos piece in
      let parts_str = 
        Piece.get_parts piece
        |> Array.map (fun (part:PP.t) -> Printf.sprintf "[%d; %d]" part.i part.j)
        |> Array.to_list
        |> String.concat "; "
      in
      Printf.sprintf "{ nb_block = %d; parts = [ %s ]; color = %s; pos = (%d, %d) }"
        (Piece.get_nb_block piece)
        parts_str
        (match Piece.get_color piece with
         | Red -> "Red"
         | Green -> "Green"
         | Blue -> "Blue"
         | Yellow -> "Yellow"
         | NoColor -> "None")
        pos_x
        pos_y)
    (QCheck.Gen.map
       (fun (x, y, z) -> Piece.create z (make_random_piece_parts z) Red (x, y))
       QCheck.Gen.(triple (int_bound 19) (int_bound 19) (int_range 1 15))) 



let test_create =
  let gen_int =  QCheck.int_bound 1000 in
  QCheck.Test.make 
  ~count:1000
  ~name:"test_create"  
  gen_int (fun i ->
      (try let b = Board.create i in
      assert_equal i b.size;
      assert_equal i (Array.length b.board.(0));
      assert_equal i (Array.length b.board);
      assert_equal 4 b.nb_players;
      assert_equal [] b.placed_pieces;
      true
  with Failure _ -> i <=0;) ;
      
    ) 


let check_board board (piece:Piece.t) color = 
  Array.for_all (fun (p:PiecePart.t) -> 
    if Board.in_matrix board (fst((Piece.get_pos piece)) + p.i) (snd((Piece.get_pos piece)) + p.j) then
      Some color = (Board.get board (fst((Piece.get_pos piece)) + p.i) (snd((Piece.get_pos piece)) + p.j))
    else
      true
  ) (Piece.get_parts piece)

let check_empty_board board = 
  Array.for_all (fun row -> Array.for_all (fun cell -> cell = Piece.NoColor) row) board


let test_in_matrix  =
  QCheck.Test.make 
  ~count:1000
  ~name:"test_in_matrix"
  QCheck.(pair int int) (fun (x, y) ->
    (try let b = Board.create 10 in
      if x >= 0 && x < 10 && y >= 0 && y < 10 then
        assert_equal true (Board.in_matrix b x y)
      else
        assert_equal false (Board.in_matrix b x y);
      true
      with Failure _ -> false))
      



  

let test_place_piece =


  
    let b = ref (Board.create 20) in
  QCheck.Test.make
    ~count:10000
    ~name:"test_place_piece"
    
    piece_gen
    (fun piece ->
      try

        let placed = Board.place_piece ~first:false !b piece in

        if placed then 
          (* check if all of the position are now of the color of the piece *)
          (assert_equal ~msg: " The piece is not placed correctly " true (check_board !b piece (Piece.get_color piece));
          true)
          
        else
         ( b := Board.create 20;
          (* check if the board is empty*)
          assert_equal ~msg: " The board is not empty " true (check_empty_board !b.board);
          true)
            with Failure msg ->
              QCheck.Test.fail_reportf " Exception raise : %s" msg)



let test_is_placeable = 
 
  let full_board = Board.create 20 in 
  Array.iteri (fun i row -> Array.iteri (fun j _ -> full_board.board.(i).(j) <- Piece.Red) row)full_board.board;
  let b = ref (Board.create 20) in
  QCheck.Test.make
    ~count:10000
    ~name:"test_is_placeable"
    piece_gen
    (fun piece ->
      try
        let full_board_placeable = Board.is_placeable  full_board piece in
        let b_placeable,p_modif = Board.is_placeable  !b piece in
        assert_equal (fst(full_board_placeable)) false;

        if b_placeable then 
          (assert_equal ~msg: " The piece is not placed correctly " true (check_board !b p_modif Piece.NoColor);
          let _ =Board.place_piece !b p_modif in ();
          true)
        else
          true
         





        
      with Failure msg ->
        QCheck.Test.fail_reportf " Exception raise : %s" msg)

let () =
  QCheck_runner.run_tests_main [
    test_create;
    test_in_matrix;
    test_place_piece;
    test_is_placeable
  ]

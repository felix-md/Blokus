
open OUnit2

open Piece

let test_create _ =
  let nb_block = 4 in
  let parts = [| [|0; 0|]; [|0; 1|]; [|1; 0|]; [|1; 1|] |] in
  let color = Piece.Red in
  let pos = (5, 5) in
  let piece = Piece.create nb_block parts color pos 23 in
  assert_equal nb_block (Piece.get_nb_block piece);
  assert_equal color (Piece.get_color piece);
  assert_equal pos (Piece.get_pos piece);
  (* Check parts *)
  let piece_parts = Piece.get_parts piece in
  assert_equal nb_block (Array.length piece_parts);
  (* Check that parts are correctly initialized *)
  assert_equal 0 piece_parts.(0).PiecePart.i;
  assert_equal 0 piece_parts.(0).PiecePart.j;
  assert_equal 0 piece_parts.(1).PiecePart.i;
  assert_equal 1 piece_parts.(1).PiecePart.j;
  assert_equal 1 piece_parts.(2).PiecePart.i;
  assert_equal 0 piece_parts.(2).PiecePart.j;
  assert_equal 1 piece_parts.(3).PiecePart.i;
  assert_equal 1 piece_parts.(3).PiecePart.j

let test_copy _ =
  let nb_block = 4 in
  let parts = [| [|0; 0|]; [|0; 1|]; [|1; 0|]; [|1; 1|] |] in
  let color = Piece.Red in
  let pos = (5, 5) in
  let piece = Piece.create nb_block parts color pos 24 in
  let piece_copy = Piece.copy piece in
  
  assert_equal (Piece.get_nb_block piece) (Piece.get_nb_block piece_copy);
  assert_equal (Piece.get_color piece) (Piece.get_color piece_copy);
  assert_equal (Piece.get_pos piece) (Piece.get_pos piece_copy);

  let parts = Piece.get_parts piece in
  let parts_copy = Piece.get_parts piece_copy in
  assert_equal (Array.length parts) (Array.length parts_copy);
  for i = 0 to Array.length parts - 1 do
    assert_equal parts.(i).PiecePart.i parts_copy.(i).PiecePart.i;
    assert_equal parts.(i).PiecePart.j parts_copy.(i).PiecePart.j;
    assert_bool "Parts are same object " (parts.(i) != parts_copy.(i));
  done;


  Piece.set_color piece_copy Piece.Blue;
  assert_bool "Color of original piece should not change" (Piece.get_color piece != Piece.get_color piece_copy);

  Piece.set_pos piece_copy (6,6);
  assert_bool "Position of original piece should not change" (Piece.get_pos piece != Piece.get_pos piece_copy)

let test_contains _ =
  let nb_block = 4 in
  let parts = [| [|0; 0|]; [|0; 1|]; [|1; 0|]; [|1; 1|] |] in
  let color = Piece.Red in
  let pos = (5, 5) in
  let piece = Piece.create nb_block parts color pos 25 in
  assert_bool "Not the same piece" (Piece.contains piece 5 5);
  assert_bool "Not the same piece" (Piece.contains piece 5 6);
  assert_bool "Not the same piece" (Piece.contains piece 6 5);
  assert_bool "Not the same piece" (Piece.contains piece 6 6);
  assert_bool "Not the same piece" (not (Piece.contains piece 4 5));
  assert_bool "Not the same piece" (not (Piece.contains piece 5 4))

let test_min_max _ =
  let nb_block = 4 in
  let parts = [| [|2; 3|]; [|2; 4|]; [|3; 3|]; [|3; 4|] |] in
  let color = Piece.Green in
  let pos = (0, 0) in
  let piece = Piece.create nb_block parts color pos 25 in
  assert_equal 2 (Piece.min_i piece);
  assert_equal 3 (Piece.min_j piece);
  assert_equal 3 (Piece.max_i piece);
  assert_equal 4 (Piece.max_j piece)

let test_flip _ =
  let nb_block = 3 in
  let parts = [| [|0; 0|]; [|0; 1|]; [|1; 1|] |] in
  let color = Piece.Blue in
  let pos = (0, 0) in
  let piece = Piece.create nb_block parts color pos  25 in
  Piece.flip piece;
  let expected_parts = [| (0,1); (0,0); (1,0) |] in
  let piece_parts = Piece.get_parts piece in
  for i = 0 to nb_block - 1 do
    assert_equal (fst expected_parts.(i)) piece_parts.(i).PiecePart.i;
    assert_equal (snd expected_parts.(i)) piece_parts.(i).PiecePart.j;
  done

let test_rotate_full_circle _ =
  let nb_block = 4 in
  let parts = [| [|0; 1|]; [|1; 1|]; [|2; 1|]; [|2; 0|] |] in
  let color = Piece.Yellow in
  let pos = (0, 0) in
  let piece = Piece.create nb_block parts color pos  24 in
  let original_parts = Array.map (fun p -> (p.(0), p.(1))) parts in
  for _ = 1 to 4 do
    Piece.rotate piece
  done;
  (* Check that the parts are back to original *)
  let piece_parts = Piece.get_parts piece in
  for i = 0 to nb_block -1 do
    assert_equal (fst original_parts.(i)) piece_parts.(i).PiecePart.i;
    assert_equal (snd original_parts.(i)) piece_parts.(i).PiecePart.j;
  done

let test_move _ =
  let nb_block = 1 in
  let parts = [| [|0; 0|] |] in
  let color = Piece.Red in
  let pos = (0, 0) in
  let piece = Piece.create nb_block parts color pos 23 in
  Piece.move piece 5 5;
  assert_equal (5, 5) (Piece.get_pos piece);
  Piece.move piece (-100) 0;
  assert_equal (5, 5) (Piece.get_pos piece)

let suite =
  "Piece Tests" >::: [
    "test_create" >:: test_create;
    "test_copy" >:: test_copy;
    "test_contains" >:: test_contains;
    "test_min_max" >:: test_min_max;
    "test_flip" >:: test_flip;
    "test_rotate_full_circle" >:: test_rotate_full_circle;
    "test_move" >:: test_move;
  ]

let () =
  run_test_tt_main suite

open QCheck
open Player
open Piece

(* Test for creating a player *)
let test_create_player =
  Test.make
    ~name:"create_player"
    QCheck.unit 
    (fun () ->
      let player = Player.create ~test:true "Alpha" Piece.Red in
      (* Verify the properties of the created player *)
    
      (Player.get_name player) = "Alpha" &&
      (Player.get_color player) = Piece.Red &&
      (Player.get_is_human player) = true &&
      Array.length (Player.get_pieces player) = 21 &&
      (Player.get_in_hand_piece player) = None &&
      (Player.get_choosing_index player) = 0 &&
      (Player.get_is_blocked player) = false &&
      (Player.get_last_placed_piece player) = None
    )

(* Test for adding and removing a piece *)
let test_add_remove_piece =
  Test.make
    ~name:"add_remove_piece"
    QCheck.unit
    (fun () ->
      let player = Player.create ~test:true "Bob" Piece.Blue in
      let piece = (Player.get_pieces player).(0) in
      Player.remove_piece piece player;
      (* Check that the piece has been removed *)
      Player.remove_piece piece player;
      let removed = not (Array.exists (fun p -> p = piece) (Player.get_pieces player)) in
      Player.add_piece piece player;
      let added = Array.exists (fun p -> p = piece) (Player.get_pieces player) in
      removed && added &&
      (* Check that the piece has been re-added *)
      Array.exists (fun p -> p = piece) (Player.get_pieces player)
    )

(* Test for choosing a piece *)
let test_choose_piece =
  Test.make
    ~name:"choose_piece"
    QCheck.unit
    (fun () ->
      let player = Player.create ~test:true "Charlie" Piece.Green in
      let piece = Player.choose_piece player in
      not (Array.exists (fun p -> p = piece) (Player.get_pieces player)) &&
 
      (Player.get_last_placed_piece player) = Some piece
    )

(* Increment Decrement Test *)
let test_increment_decrement_index =
  Test.make
    ~name:"increment_decrement_index"
    QCheck.unit
    (fun () ->
      let player = Player.create ~test:true "Delta" Piece.Yellow in
      let initial_index = (Player.get_choosing_index player) in
      Player.increment_index player;
      let expected_incremented = (initial_index + 1) mod (Array.length (Player.get_pieces player)) in
      let incremented_correctly = (Player.get_choosing_index player) = expected_incremented in
      Player.decrement_index player;
      let decremented_correctly = (Player.get_choosing_index player) = initial_index in
      incremented_correctly && decremented_correctly
    )

(* Test suite *)
let suite =
  [
    test_create_player;
    test_add_remove_piece;
    test_choose_piece;
    test_increment_decrement_index;
  ]

(* Execute the test suite *)
let () =
  QCheck_runner.run_tests_main suite

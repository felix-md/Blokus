

open Game_controller



module GC = Game_controller

open Player
open OUnit2

let test_read_single_char =
  let gen_char = QCheck.char in

  QCheck.Test.make ~count:1000 ~name:"test_read_single_char" 
    gen_char
    (fun char ->
      let input = Bytes.of_string (String.make 1 char) in
      let (read_fd, write_fd) = Unix.pipe () in
      let _ = Unix.write write_fd input 0 1 in
      Unix.close write_fd;
      let old_stdin = Unix.dup Unix.stdin in
      Unix.dup2 read_fd Unix.stdin;
      Unix.close read_fd;
      let result = GC.read_single_char () in
      let success = result = char in
      Unix.dup2 old_stdin Unix.stdin;
      Unix.close old_stdin;
      if not success then
        Printf.printf "Test failed for character: %c\n" char;
      success
    )





    let test_handle_piece_selection_input =
      QCheck.Test.make
        ~count:1
        ~name:"test_handle_piece_selection_input"
        QCheck.unit
        (fun () ->
          (* Test right arrow *)
          let (read_fd, write_fd) = Unix.pipe () in
          let _ = Unix.write write_fd (Bytes.of_string "\027[C") 0 3 in
          Unix.close write_fd;
          
          let old_stdin = Unix.dup Unix.stdin in
          Unix.dup2 read_fd Unix.stdin;
          Unix.close read_fd;
          
          let player = Player.create ~test:true "Player 1" Red in
          let initial_index = (Player.get_choosing_index player) in
          let result = GC.handle_piece_selection_input player in
          
          Unix.dup2 old_stdin Unix.stdin;
          Unix.close old_stdin;
          
          assert_equal ~msg:"Right arrow should return Continue" 
            `Continue result;
          assert_equal ~msg:"Choosing index should increment" 
            (initial_index + 1) (Player.get_choosing_index player);
    
          (* Test space key *)
          let (read_fd, write_fd) = Unix.pipe () in
          let _ = Unix.write write_fd (Bytes.of_string " ") 0 1 in
          Unix.close write_fd;
          
          let old_stdin = Unix.dup Unix.stdin in
          Unix.dup2 read_fd Unix.stdin;
          Unix.close read_fd;
          
          let player = Player.create ~test:true "Player 2" Red in
          (Player.set_choosing_index player 1 );
          let selected_piece = (Player.get_pieces player).(1) in
          let result = GC.handle_piece_selection_input player in
          
          Unix.dup2 old_stdin Unix.stdin;
          Unix.close old_stdin;
          
          assert_equal ~msg:"Space should return PieceSelected" 
            `PieceSelected result;
          assert_equal ~msg:"Choosing index should reset to 0" 
            0 (Player.get_choosing_index player);
          assert_equal ~msg:"Selected piece should be in hand" 
            (Some selected_piece) (Player.get_in_hand_piece player);
          assert_equal ~msg:"Pieces array should be one shorter" 
            21 (Array.length (Player.get_pieces player));
          
          true)



let () =

  QCheck_runner.run_tests_main [
    test_read_single_char;
    test_handle_piece_selection_input
  ]
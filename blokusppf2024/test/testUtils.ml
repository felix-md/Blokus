(* test_utils.ml *)

open QCheck

open Utils

let arb_coord =
  let open Gen in
  pair (int_range (-1000) 1000) (int_range (-1000) 1000)

let arb_piece =
  let open Gen in
  list_size (1 -- 10) arb_coord

let arb_pieces =
  let open Gen in
  list_size (1 -- 5) arb_piece

(* Test for the load_pieces function *)
let test_load_pieces =
  Test.make
    ~name:"Test Utils.load_pieces"
    (make arb_pieces)
    (fun pieces ->
      (* Convert pieces to JSON structure *)
      let json = `Assoc [
        "pieces", `List (
          List.map (fun piece ->
            `List (List.map (fun (i, j) ->
              `List [`Int i; `Int j]
            ) piece)
          ) pieces
        )
      ] in
      (* Convert JSON structure to string *)
      let json_str = Yojson.Basic.to_string json in
      let temp_file = Filename.temp_file "pieces" ".json" in
      let oc = open_out temp_file in
      output_string oc json_str;
      close_out oc;
      let pieces_read = Utils.load_pieces temp_file in
      Sys.remove temp_file;
      pieces = pieces_read
    )

(* Run the test *)
let () =
  QCheck_runner.run_tests_main [
    test_load_pieces
  ]

(* util.ml *)

module Utils = struct
 
  (** [load_pieces json_file] loads the pieces from a JSON file.

    @param json_file The path to the JSON file containing the pieces.
    @return A list of pieces, where each piece is represented as a list of coordinates (i, j).
    
    The JSON file is expected to have the following structure:
    {
      "pieces": [
      [[i1, j1], [i2, j2], ...],
      ...
      ]
    }
    
    Each piece is a list of coordinates, and each coordinate is a pair of integers.
  *)
  let load_pieces json_file =
    let json = Yojson.Basic.from_file json_file in
    let open Yojson.Basic.Util in
    json |> member "pieces" |> to_list |> List.map (fun piece ->
      piece |> to_list |> List.map (fun coord ->
        let i = coord |> index 0 |> to_int in
        let j = coord |> index 1 |> to_int in
        (i, j)
      )
    )



end

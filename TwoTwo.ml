exception Found of string * string

let main () =
  let strings =
    BatIO.lines_of BatIO.stdin
    |> BatList.of_enum
  in
  try
    BatList.iter (fun str1 ->
        BatList.iter (fun str2 ->
          match BatString.edit_distance str1 str2 with
            | 1 ->
              raise (Found (str1, str2))
            | _ ->
              ()
          )
          strings
      )
      strings
  with
  | Found (str1, str2) ->
    BatEnum.combine ((BatString.enum str1), (BatString.enum str2))
    |> BatEnum.filter_map
      (fun (left, right) ->
         if left = right then
          Some left
         else
           None
      )
    |> BatString.of_enum
    |> BatIO.nwrite BatIO.stdout

let _ = AOCUtils.Time.time main




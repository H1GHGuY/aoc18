exception Found of string * string

let try_edit_distance element others =
        BatList.iter (fun other ->
          match BatString.edit_distance element other with
            | 1 ->
              raise (Found (element, other))
            | _ ->
              ()
          )
          others

let rec find lst =
  match lst with
  | [] | [_] -> raise Not_found
  | hd::tl ->
    let () = try_edit_distance hd tl in
    find tl

let main () =
  let strings =
    BatIO.lines_of BatIO.stdin
    |> BatList.of_enum
  in
  try
    find strings
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




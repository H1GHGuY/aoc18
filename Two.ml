let count arr line =
  let () =
    BatString.iter
      (fun c ->
         let index = int_of_char c in
         BatArray.set arr index (BatArray.get arr index + 1)
      )
      line
  in
  BatArray.fold_left (fun (doubles, triples) count ->
      match count with
      | 2 ->
        (doubles + 1, triples)
      | 3 ->
        (doubles, triples + 1)
      | _ ->
        (doubles, triples)
    )
    (0, 0)
    arr


let main () =
  BatIO.lines_of BatIO.stdin
  |> BatEnum.fold
    (fun (doubles, triples, tmp) line ->
       let (new_doubles, new_triples) = count tmp line in
       let () = BatArray.fill tmp 0 256 0 in
       ((min new_doubles 1) + doubles,
        (min new_triples 1) + triples,
        tmp)
    )
    (0, 0, BatArray.make 256 0)
  |> (fun (doubles, triples, _) -> string_of_int @@ doubles * triples)
  |> BatIO.nwrite BatIO.stdout

let _ = AOCUtils.Time.time main

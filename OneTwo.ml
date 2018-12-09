exception Completed of int

let main () =
  try
    BatIO.lines_of BatIO.stdin
    |> BatEnum.map int_of_string
    |> BatEnum.cycle
    |> BatEnum.fold (fun (acc, seen) x ->
        let cur = acc + x in
        let seen =
          if BatSet.Int.mem cur seen then
            raise (Completed cur)
          else
            BatSet.Int.add cur seen
        in
        (cur, seen)
      )
      (0, BatSet.Int.empty)
    |> BatPervasives.const ()
  with
  | Completed x ->
    BatIO.nwrite BatIO.stdout (string_of_int x)

let _ = AOCUtils.Time.time main

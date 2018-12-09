let main () =
  BatIO.lines_of BatIO.stdin
  |> BatEnum.map int_of_string
  |> BatEnum.sum
  |> string_of_int
  |> BatIO.nwrite BatIO.stdout

let _ = AOCUtils.Time.time main

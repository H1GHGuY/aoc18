open BatPervasives

module Matrix = BatBigarray.Array2

let claim matrix (_id, left, top, width, height) =
  for x = left to left + width - 1 do
    for y = top to top + height - 1 do
      let v = Matrix.get matrix x y in
      Matrix.set matrix x y (v + 1)
    done
  done;
  matrix

let overlap matrix =
  Matrix.enum matrix
  |> BatEnum.filter (fun x -> x > 1)
  |> BatEnum.count

let parse line =
  let (id, claim ) = BatString.split line ~by:"@" in
  let (offset, area) = BatString.split claim ~by:":" in
  let (left, top) = BatString.split offset ~by:"," in
  let (width, height) = BatString.split area ~by:"x" in
  (int_of_string @@ BatString.trim @@ BatString.tail id 1,
   int_of_string @@ BatString.trim left,
   int_of_string @@ BatString.trim top,
   int_of_string @@ BatString.trim width,
   int_of_string @@ BatString.trim height)

let main () =
  let fabric = Matrix.create BatBigarray.Int BatBigarray.c_layout 1000 1000 in
  let () = Matrix.fill fabric 0 in
  BatIO.lines_of BatIO.stdin
  |> BatEnum.map parse
  |> BatEnum.fold claim fabric
  |> overlap
  |> string_of_int
  |> BatIO.nwrite BatIO.stdout

let _ = main ()

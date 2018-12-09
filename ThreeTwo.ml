open BatPervasives

module Matrix = BatBigarray.Array2

let claim (no_overlap, matrix) (id, left, top, width, height) =
  let end_column = left + width in
  let end_row = top + height in
  let rec horizontal (no_overlap, matrix, overlapped) row column =
    if end_column = column then
      (no_overlap, matrix, overlapped)
    else
      let v = Matrix.unsafe_get matrix row column in
      let () = Matrix.unsafe_set matrix row column id in
      let next =
        if v <> 0 then
          (BatSet.remove v no_overlap,
           matrix,
           true)
        else
          (no_overlap,
           matrix,
           overlapped)
      in
      horizontal next row (column + 1)
  in
  let rec vertical acc row =
    if row = end_row then
      acc
    else
      let next = horizontal acc row left in
      vertical next (row + 1)
  in
  let (no_overlap, matrix, overlapped) =
    vertical
      (no_overlap, matrix, false)
      top
  in
  if overlapped then
    (no_overlap, matrix)
  else
    (BatSet.add id no_overlap, matrix)

let no_overlap (set, _fabric) =
  fst @@ BatSet.pop_min set

let parse line =
  let convert str = int_of_string @@ BatString.trim str in
  let (id, claim ) = BatString.split line ~by:"@" in
  let (offset, area) = BatString.split claim ~by:":" in
  let (left, top) = BatString.split offset ~by:"," in
  let (width, height) = BatString.split area ~by:"x" in
  (convert @@ BatString.tail id 1,
   convert left,
   convert top,
   convert width,
   convert height)

let main () =
  let fabric = Matrix.create BatBigarray.Int BatBigarray.c_layout 1000 1000 in
  let () = Matrix.fill fabric 0 in
  BatIO.lines_of BatIO.stdin
  |> BatEnum.map parse
  |> BatEnum.fold claim (BatSet.empty, fabric)
  |> no_overlap
  |> string_of_int
  |> BatIO.nwrite BatIO.stdout

let _ = AOCUtils.Time.time main

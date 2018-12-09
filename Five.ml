open BatPervasives

let reduce_once lst =
  let rec aux acc lst =
    match (lst, acc) with
    | [], _ ->
      acc
    | hd :: tl, [] ->
      aux [hd] tl
    | hd :: tl, previous :: acc_tl ->
      if previous <> hd && ((BatChar.uppercase_ascii hd) = (BatChar.uppercase_ascii previous)) then
        aux acc_tl tl
      else
        aux (hd :: acc) tl
  in
  aux [] lst

let reduce lst =
  let rec once lst length =
    let new_lst = reduce_once lst in
    let new_length = BatList.length new_lst in
    if new_length <> length then
      once new_lst new_length
    else
      new_length
  in
  let length = BatList.length lst in
  once lst length
  

let main () =
  let data =
    BatIO.chars_of BatIO.stdin
    |> BatEnum.filter BatChar.is_letter
    |> BatList.of_enum
  in
 
  let size = reduce data in
  BatIO.nwrite BatIO.stdout (string_of_int size)

let _ = AOCUtils.Time.time main

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
 
  BatChar.range 'a' ~until:'z'
  |> BatEnum.map
    (fun lowercase ->
       let uppercase = BatChar.uppercase_ascii lowercase in
       let filtered_list = BatList.filter (fun c -> c <> lowercase && c <> uppercase) data in
       let size = reduce filtered_list in
       size
    )
  |> BatEnum.reduce BatInt.min
  |> string_of_int
  |> BatIO.nwrite BatIO.stdout

let _ = AOCUtils.Time.time main

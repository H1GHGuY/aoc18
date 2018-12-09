open BatPervasives

type player = { score: int }
type players = player BatMap.Int.t

let high_score players =
  BatMap.Int.fold
    (fun _id player current_high ->
       BatInt.max player.score current_high
    )
    players
    0

let create_players count =
    let empty = { score = 0; } in
    BatEnum.init count (fun i -> (i + 1, empty))
    |> BatMap.Int.of_enum

let add_score player players score =
  BatMap.Int.modify
    player
    (fun cur -> { score = cur.score + score })
    players

let next_player cur_player players =
  let (last_player, _) = BatMap.Int.max_binding players in
  if cur_player = last_player then
    1
  else
    cur_player + 1

let show_players =
  BatMap.Int.iter (fun id player ->
      Printf.printf "%d: %d points" id player.score
    )



type field = int BatDllist.t

let create_field () =
  BatDllist.create 0

let simple_insert field marble =
  let next = BatDllist.next field in
  BatDllist.append next marble

let rec skip field count =
  if count = 0 then
    field
  else if count < 0 then
    skip (BatDllist.prev field) (count + 1)
  else
    skip (BatDllist.next field) (count - 1)

let remove_special field =
  let to_remove = skip field (-7) in
  let removed = BatDllist.get to_remove in
  (BatDllist.drop to_remove, removed)

let show_field =
  BatDllist.iter (fun cur_node ->
      Printf.printf "%d " cur_node
    )



let single_turn (cur_player, players, field) turn =
  (*
  let () = show_field field in
  let () = Printf.printf "\nturn: %d\n" turn in
   *)
  let next = next_player cur_player players in
  if (BatInt.rem turn 23) = 0 then
    let (field, score) = remove_special field in
    let score = score + turn in
    (next,
     add_score cur_player players score,
     field)
  else
    (next,
     players,
     simple_insert field turn)

let main () =
  let (n_players, n_marbles) = (459, 72103 * 100) in
  let players = create_players n_players in
  let field = create_field () in
  let turns = BatEnum.init (n_marbles - 1) (fun x -> x + 1) in
  let (_, players, _) =
    BatEnum.fold
      single_turn
      (1, players, field)
      turns
  in
  let winning_score = high_score players in
  BatIO.nwrite BatIO.stdout (string_of_int winning_score)


let () = main ()

open BatPervasives

type guardid = int
type date = { year: int
            ; month: int
            ; day: int
            ; hour: int
            ; minute: int
            }

type event =
  | StartShift of guardid * date
  | Sleep of date
  | Wake of date

let date_of_event = function
  | StartShift (_, date) -> date
  | Sleep date -> date
  | Wake date -> date

let compare_event left right =
  compare (date_of_event left) (date_of_event right)

module EventSet = BatSet.Make (struct
    type t = event
    let compare = compare_event
  end)

let parse line =
  let date = BatString.left line 17 in
  let event = BatString.lchop ~n:19 line in

  BatScanf.sscanf date "[%d-%d-%d %d:%d" (fun year month day hour minute ->
      let time = { year; month; day; hour; minute } in
      match BatString.get event 0 with
      | 'G' ->
        let guardid = BatString.lchop ~n:7 event in
        let (guardid, _) = BatString.split guardid ~by:" " in
        StartShift (int_of_string guardid, time)
      | 'w' ->
        Wake time
      | 'f' ->
        Sleep time
      | _ ->
        raise Not_found
    )

type guardsleep = int BatArray.t
type guardlist = (guardid, guardsleep) BatHashtbl.t

type state =
  | NobodyHere
  | GuardAsleep of guardid * date
  | GuardAwake of guardid

let set_asleep tbl guardid starttime endtime =
  BatHashtbl.modify_opt
    guardid
    (fun arr_opt ->
       let arr =
         match arr_opt with
         | None -> 
           (BatArray.make 60 0)
         | Some arr -> 
           arr
       in
       for i = starttime.minute to endtime.minute - 1 do
         let v = BatArray.get arr i in
         BatArray.set arr i (v+1)
       done;
       Some arr
    )
    tbl

let build event (state, tbl) =
  match state, event with
  | NobodyHere, StartShift (guardid, _time) ->
    (GuardAwake guardid), tbl
  | (GuardAwake _guardid), StartShift (new_guardid, _time) ->
    (GuardAwake new_guardid), tbl
  | GuardAsleep (guardid, starttime), Wake endtime ->
    let () = set_asleep tbl guardid starttime endtime in
    GuardAwake guardid, tbl
  | GuardAwake guardid, Sleep starttime ->
    GuardAsleep (guardid, starttime), tbl
  | _, _ -> raise Not_found


let evaluate (state, tbl) =
  let () =
    match state with
    | GuardAsleep (_guardid, _time) -> raise Not_found
    | _ -> ()
  in

  let (sleeper_guard, _max_sleep, arr) =
    BatHashtbl.enum tbl
    |> BatEnum.map (fun (guardid, arr) -> (guardid, BatArray.sum arr, arr))
    |> BatEnum.fold (fun (max_guard, max_sleep, max_arr) (guard, sleep, arr) ->
        if sleep > max_sleep then
          (guard, sleep, arr)
        else
          (max_guard, max_sleep, max_arr)
      )
      (0, 0, BatArray.make 0 0)
  in
  let max_sleep_minute = BatArray.max arr in
  let minute = BatArray.findi (fun x -> x = max_sleep_minute) arr in
  sleeper_guard * minute

let main () =
  BatIO.lines_of BatIO.stdin
  |> BatEnum.map parse
  |> EventSet.of_enum
  |> (fun eventset -> EventSet.fold build eventset (NobodyHere, BatHashtbl.create 50))
  |> evaluate
  |> string_of_int
  |> BatIO.nwrite BatIO.stdout

let _ = AOCUtils.Time.time main

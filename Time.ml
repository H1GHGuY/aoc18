let time f =
  let start = BatUnix.gettimeofday () in
  let res = f () in
  let stop = BatUnix.gettimeofday () in
  let () = Printf.eprintf "\nTime: %f\n" (stop -. start) in
  res

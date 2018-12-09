let time f =
  let gc = BatGc.get () in
  let gc = { gc with
             minor_heap_size = 5 * 1024 * 1024 / 8;
           }
  in
  let () = BatGc.set gc in

  let start = BatUnix.gettimeofday () in
  let res = f () in
  let stop = BatUnix.gettimeofday () in
  let () = Printf.eprintf "\nTime: %f\n" (stop -. start) in
  let () = BatGc.print_stat BatIO.stderr in
  res

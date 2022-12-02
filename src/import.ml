let id_print f x =
  printf "%s\n" (f x);
  x
;;

let timeit f =
  let tick = Time.now () in
  let res = f () in
  let tock = Time.now () in
  let diff = Time.abs_diff tick tock in
  res, diff
;;
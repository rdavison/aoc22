open! Import

let path_to x =
  match Sites.Sites.puzzle_inputs with
  | [ path ] -> path ^/ x
  | _ -> failwithf "No path to puzzle input: %s" x ()
;;

let day1_test1 = path_to "day1-test1"
let day1_1 = path_to "day1-1"
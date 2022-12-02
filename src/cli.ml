open! Import

let main day =
  match day with
  | 1 -> Day1.main ()
  | _ ->
    if day >= 1 && day <= 25
    then failwithf "Not implemented: day %d" day ()
    else failwithf "Unknown: day %d" day ()
;;

let cmd =
  let param =
    let open Command.Let_syntax in
    let%map day = Command.Param.(anon ("day" %: int)) in
    fun () -> main day
  in
  Command.basic ~summary:"Advent of Code - 2022" param
;;

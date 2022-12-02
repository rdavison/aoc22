open! Import

let cmd =
  let param =
    let open Command.Param in
    return (fun () -> print_endline "hello world")
  in
  Command.basic ~summary:"Advent of Code - 2022" param
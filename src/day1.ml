open! Import

let group_items_by_elf lst =
  let elves, last_elf =
    List.fold lst ~init:([], []) ~f:(fun (elves, elf) item ->
        match item with
        | "" -> elf :: elves, []
        | num ->
          let item = Int.of_string num in
          elves, item :: elf)
  in
  List.rev (last_elf :: elves)
;;

let count_calories = List.sum (module Int) ~f:Fn.id

let shared_runtime input_path =
  In_channel.read_all input_path
  |> String.split_lines
  |> List.map ~f:String.strip
  |> group_items_by_elf
  |> List.map ~f:count_calories
;;

let part1 input_path =
  let run () =
    shared_runtime input_path
    |> List.max_elt ~compare:Int.compare
    |> Option.value ~default:0
  in
  let to_string = sprintf "%d" in
  { Exec.Part.name = "part1"; run; to_string }
;;

let part2 input_path =
  let run () =
    shared_runtime input_path
    |> List.sort ~compare:(Comparable.reverse Int.compare)
    |> Fn.flip List.take 3
    |> count_calories
  in
  let to_string = sprintf "%d" in
  { Exec.Part.name = "part2"; run; to_string }
;;

module For_test = struct
  let main ?no_time x = Exec.many ?no_time [ part1 x; part2 x ]
end

let main () = For_test.main Puzzle_inputs.day1_1

let%expect_test "day1-example" =
  For_test.main ~no_time:true Puzzle_inputs.day1_test1;
  [%expect
    {|
    -- Running: part1
    -- Result:
    24000
    -- Running: part2
    -- Result:
    45000 |}]
;;

let%expect_test "day1-actual" =
  For_test.main ~no_time:true Puzzle_inputs.day1_1;
  [%expect
    {|
    -- Running: part1
    -- Result:
    72240
    -- Running: part2
    -- Result:
    210957 |}]
;;
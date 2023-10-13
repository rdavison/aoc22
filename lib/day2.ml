open! Import

type shape =
  [ `Rock
  | `Paper
  | `Scissors
  ]
[@@deriving sexp]

type outcome =
  [ `Win
  | `Lose
  | `Draw
  ]
[@@deriving sexp]

type 'kind round = shape * 'kind [@@deriving sexp]
type 'kind strategy_guide = 'kind round list [@@deriving sexp]

let parse_shape = function
  | "A" | "X" -> `Rock
  | "B" | "Y" -> `Paper
  | "C" | "Z" -> `Scissors
  | other -> failwiths "Invalid symbol" other [%sexp_of: string] ~here:[%here]
;;

let parse_outcome = function
  | "X" -> `Lose
  | "Y" -> `Draw
  | "Z" -> `Win
  | other -> failwiths "Invalid symbol" other [%sexp_of: string] ~here:[%here]
;;

let play_shape_strategy player ~opponent =
  match player, opponent with
  | `Rock, `Scissors | `Scissors, `Paper | `Paper, `Rock -> `Win
  | `Rock, `Rock | `Paper, `Paper | `Scissors, `Scissors -> `Draw
  | _, _ -> `Lose
;;

let play_outcome_strategy player ~opponent =
  match player, opponent with
  | `Win, `Rock -> `Paper
  | `Win, `Paper -> `Scissors
  | `Win, `Scissors -> `Rock
  | `Lose, `Rock -> `Scissors
  | `Lose, `Paper -> `Rock
  | `Lose, `Scissors -> `Paper
  | `Draw, shape -> shape
;;

let score_shape = function
  | `Rock -> 1
  | `Paper -> 2
  | `Scissors -> 3
;;

let score_outcome = function
  | `Win -> 6
  | `Draw -> 3
  | `Lose -> 0
;;

let%expect_test "part 1" =
  let strategy_guide : shape strategy_guide =
    [%blob "inputs/day2.txt"]
    |> String.split_lines
    |> List.map ~f:String.strip
    |> List.map ~f:(String.split_on_chars ~on:[ ' ' ])
    |> List.filter_map ~f:(function
      | [ opponent; player ] -> Some (parse_shape opponent, parse_shape player)
      | _ -> None)
  in
  let score =
    List.fold strategy_guide ~init:0 ~f:(fun score (opponent, player) ->
      let shape_score = score_shape player in
      let outcome_score = score_outcome (play_shape_strategy player ~opponent) in
      score + shape_score + outcome_score)
  in
  printf "%d" score;
  [%expect {| 9241 |}]
;;

let%expect_test "part 2" =
  let strategy_guide : outcome strategy_guide =
    [%blob "inputs/day2.txt"]
    |> String.split_lines
    |> List.map ~f:String.strip
    |> List.map ~f:(String.split_on_chars ~on:[ ' ' ])
    |> List.filter_map ~f:(function
      | [ opponent; player ] -> Some (parse_shape opponent, parse_outcome player)
      | _ -> None)
  in
  let score =
    List.fold strategy_guide ~init:0 ~f:(fun score (opponent, player) ->
      let outcome_score = score_outcome player in
      let shape_score = score_shape (play_outcome_strategy player ~opponent) in
      score + shape_score + outcome_score)
  in
  printf "%d" score;
  [%expect {| 14610 |}]
;;

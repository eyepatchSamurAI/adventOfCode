open Core

let _my_print_string chars =
  let str = String.of_char_list chars in
  Fmt.pr "%s@." str
;;

let word_to_number word =
  match word with
  | "one" -> 1
  | "two" -> 2
  | "three" -> 3
  | "four" -> 4
  | "five" -> 5
  | "six" -> 6
  | "seven" -> 7
  | "eight" -> 8
  | "nine" -> 9
  | _ -> -0
;;

let process_line line is_rev =
  let rec aux acc chars =
    match chars with
    | [] -> word_to_number acc
    | c :: cs ->
      (* Fmt.pr "%s@." acc; *)
      if Char.is_digit c
      then Char.get_digit_exn c
      else (
        let new_acc =
          if is_rev then Char.to_string c ^ acc else acc ^ Char.to_string c
        in
        let found =
          List.find
            [ "one"
            ; "two"
            ; "three"
            ; "four"
            ; "five"
            ; "six"
            ; "seven"
            ; "eight"
            ; "nine"
            ]
            ~f:(fun word -> String.is_substring new_acc ~substring:word)
        in
        match found with
        | Some value -> word_to_number value
        | None -> aux new_acc cs)
  in
  aux "" (String.to_list line)
;;

let () =
  let lines = Advent.read_lines "../../data/year2023/challenge1/puzzle.txt" in
  List.fold lines ~init:0 ~f:(fun acc line ->
    let chars = String.to_list line in
    let numbers = List.filter chars ~f:Char.is_digit in
    let digit = Fmt.str "%c%c" (List.hd_exn numbers) (List.last_exn numbers) in
    acc + Int.of_string digit)
  |> Fmt.pr "@Part 1 Result: %d@."
;;

let () =
  let lines = Advent.read_lines "../../data/year2023/challenge1/puzzle.txt" in
  let first = List.map lines ~f:(fun line -> process_line line false) in
  Fmt.pr "";
  let last =
    List.map lines ~f:(fun line -> process_line (String.rev line) true)
  in
  Fmt.pr "";
  let zipped = List.map2 first last ~f:(fun f l -> (f * 10) + l) in
  match zipped with
  | Ok zip ->
    let sum = List.fold zip ~init:0 ~f:(fun acc a -> acc + a) in
    Fmt.pr "@Part 2 Result: %d@." sum
  | Unequal_lengths -> Fmt.pr "Lists have unequal lengths@."
;;

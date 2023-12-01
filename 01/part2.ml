open Base
open Stdio

let digits =
  [
    ("one", "1");
    ("two", "2");
    ("three", "3");
    ("four", "4");
    ("five", "5");
    ("six", "6");
    ("seven", "7");
    ("eight", "8");
    ("nine", "9");
  ]

let convert_digits str =
  List.fold ~init:str
    ~f:(fun str (digit, number) ->
      String.substr_replace_all ~pattern:digit
        ~with_:(digit ^ number ^ digit)
        str)
    digits

let () =
  let lines = In_channel.read_lines "input.txt" in
  let numbers =
    List.map
      ~f:(fun line ->
        let chars = String.to_list (convert_digits line) in
        let numbers = List.filter ~f:Char.is_digit chars in
        let first_number = List.hd numbers in
        let last_number = List.last numbers in
        match (first_number, last_number) with
        | Some x, Some y -> Int.of_string (String.make 1 x ^ String.make 1 y)
        | Some x, _ -> Int.of_string (String.make 1 x)
        | _ -> 0)
      lines
  in
  let sum = List.fold ~f:( + ) ~init:0 numbers in
  printf "%d\n" sum

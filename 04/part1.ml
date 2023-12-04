open Base
open Stdio

let rec get_points = function
  | n when n <= 1 -> n
  | n -> 2 * get_points (n - 1)

let get_numbers str =
  str |> String.split ~on:' ' |> List.filter_map ~f:Int.of_string_opt

let () =
  let lines = In_channel.read_lines "input.txt" in
  let points =
    lines
    |> List.map ~f:(fun line ->
           match String.split ~on:':' line with
           | [_; y] -> (
             match String.split ~on:'|' y with
             | [numbers; winners] ->
               let numbers_set =
                 Hash_set.of_list (module Int) (get_numbers numbers)
               in
               let winners_set =
                 Hash_set.of_list (module Int) (get_numbers winners)
               in
               let intersection = Hash_set.inter numbers_set winners_set in
               Hash_set.length intersection
             | _ -> 0)
           | _ -> 0)
    |> List.map ~f:get_points
  in
  let sum = List.sum ~f:Fn.id (module Int) points in
  printf "%d\n" sum

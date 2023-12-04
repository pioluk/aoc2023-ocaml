open Base
open Stdio

let get_numbers str =
  str |> String.split ~on:' ' |> List.filter_map ~f:Int.of_string_opt

let rec get_cards card_map (number, count) =
  let child_cards =
    List.range ~start:`exclusive ~stop:`inclusive number (number + count)
    |> List.map ~f:(fun key -> (key, Map.find_exn card_map key))
    |> List.concat_map ~f:(fun card -> get_cards card_map card)
  in
  number :: child_cards

let () =
  let lines = In_channel.read_lines "input.txt" in
  let cards_data =
    List.filter_mapi
      ~f:(fun i line ->
        match String.split ~on:':' line with
        | [_; y] ->
          let winning_numbers =
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
            | _ -> 0
          in
          Some (i + 1, winning_numbers)
        | _ -> None)
      lines
  in
  let cards_map = Map.of_alist_exn (module Int) cards_data in
  let all_cards = List.concat_map ~f:(get_cards cards_map) cards_data in
  printf "%d\n" @@ List.length all_cards

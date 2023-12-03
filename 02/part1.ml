open Base
open Stdio

let min_red = 12
let min_green = 13
let min_blue = 14

let () =
  let lines = In_channel.read_lines "input.txt" in
  let ids =
    List.filter_map
      ~f:(fun line ->
        match String.split ~on:':' line with
        | [game; cubes] ->
          let game_id =
            Int.of_string
            @@ String.substr_replace_all ~pattern:"Game " ~with_:"" game
          in
          let cubes = String.substr_replace_all ~pattern:";" ~with_:"," cubes in
          let cubes = String.split ~on:',' cubes in
          let colored_cubes =
            List.filter_map
              ~f:(fun cube ->
                let cube =
                  String.substr_replace_first ~pattern:" " ~with_:"" cube
                in
                match String.split ~on:' ' cube with
                | [count; color] -> Some (color, Int.of_string count)
                | _ -> None)
              cubes
          in
          let max_red =
            Option.value ~default:0
            @@ List.max_elt ~compare:Int.compare
            @@ List.filter_map
                 ~f:(function
                   | "red", count -> Some count
                   | _ -> None)
                 colored_cubes
          in
          let max_green =
            Option.value ~default:0
            @@ List.max_elt ~compare:Int.compare
            @@ List.filter_map
                 ~f:(function
                   | "green", count -> Some count
                   | _ -> None)
                 colored_cubes
          in
          let max_blue =
            Option.value ~default:0
            @@ List.max_elt ~compare:Int.compare
            @@ List.filter_map
                 ~f:(function
                   | "blue", count -> Some count
                   | _ -> None)
                 colored_cubes
          in
          if
            max_red <= min_red && max_green <= min_green && max_blue <= min_blue
          then Some game_id
          else None
        | _ -> None)
      lines
  in
  let sum = List.sum ~f:(fun x -> x) (module Int) ids in
  printf "%d\n" sum

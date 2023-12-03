open Base
open Stdio

let () =
  let lines = In_channel.read_lines "input.txt" in
  let ids =
    List.filter_map
      ~f:(fun line ->
        match String.split ~on:':' line with
        | [_; cubes] ->
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
          let red_cubes, green_cubes, blue_cubes =
            List.partition3_map
              ~f:(function
                | "red", count -> `Fst count
                | "green", count -> `Snd count
                | "blue", count -> `Trd count
                | _ -> `Trd 0)
              colored_cubes
          in
          let max_red =
            Option.value ~default:0
            @@ List.max_elt ~compare:Int.compare red_cubes
          in
          let max_green =
            Option.value ~default:0
            @@ List.max_elt ~compare:Int.compare
            @@ green_cubes
          in
          let max_blue =
            Option.value ~default:0
            @@ List.max_elt ~compare:Int.compare
            @@ blue_cubes
          in
          Some (max_red * max_green * max_blue)
        | _ -> None)
      lines
  in
  let sum = List.sum ~f:(fun x -> x) (module Int) ids in
  printf "%d\n" sum

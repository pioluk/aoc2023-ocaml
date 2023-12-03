open Base
open Stdio

module Point = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp_of]
  end

  include T
  include Comparable.Make (T)
end

let moves =
  [(-1, 1); (0, 1); (1, 1); (-1, 0); (1, 0); (-1, -1); (0, -1); (1, -1)]

let is_symbol c = Char.(c <> '.') && (not @@ Char.is_digit c)

let () =
  let lines = In_channel.read_lines "input.txt" in
  let symbols =
    List.foldi
      ~f:(fun j acc line ->
        List.foldi
          ~f:(fun i acc c -> if is_symbol c then Set.add acc (i, j) else acc)
          ~init:acc (String.to_list line))
      ~init:(Set.empty (module Point))
      lines
  in
  let _, numbers =
    List.foldi
      ~f:(fun j acc line ->
        List.foldi
          ~f:(fun i ((current_number, positions), acc) c ->
            if Char.is_digit c then
              ((current_number ^ String.of_char c, (i, j) :: positions), acc)
            else
              ( ("", []),
                if String.is_empty current_number then acc
                else (Int.of_string current_number, positions) :: acc ))
          ~init:acc (String.to_list line))
      ~init:(("", []), [])
      lines
  in
  let parts =
    List.filter_map
      ~f:(fun (value, positions) ->
        let neighbors =
          Set.of_list (module Point)
          @@ List.concat_map
               ~f:(fun (x, y) ->
                 List.map ~f:(fun (dx, dy) -> (x + dx, y + dy)) moves)
               positions
        in
        let union = Set.inter symbols neighbors in
        if Set.is_empty union then None else Some value)
      numbers
  in
  let sum = List.sum ~f:(fun x -> x) (module Int) parts in
  printf "%d\n" sum

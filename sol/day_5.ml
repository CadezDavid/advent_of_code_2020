#load "unix.cma";;

let day = "5"

let rec power a b =
  if b = 0 then 1 else
  if b > 0 then a * power a (b - 1)
  else failwith "What is a fraction."

let rec clean = function
  | [] -> []
  | x :: xs ->
    if x = "" then clean xs
    else x :: clean xs

let row_and_col s =
  let rec aux row col i s =
    if i > 9 then (row, col)
    else match s.[i] with
      | 'B' -> aux (row + power 2 (6 - i)) col (i + 1) s
      | 'F' -> aux row col (i + 1) s
      | 'R' -> aux row (col + power 2 (9 - i)) (i + 1) s
      | 'L' -> aux row col (i + 1) s
      | _ -> failwith "Unexpected character in row_and_col."
  in
  aux 0 0 0 s

let seat_id s =
  let (row, col) = row_and_col s in
  ((8 * row) + col)

let rec sort list =
  let rec is_sorted = function
    | [] | [_] -> true
    | x1 :: x2 :: xs ->
      if x1 <= x2 then is_sorted (x2 :: xs)
      else false
  in
  let rec sort_aux = function
    | [] -> []
    | [x] -> [x]
    | x1 :: x2 :: xs ->
      if x1 < x2 then x1 :: sort_aux (x2 :: xs)
      else x2 :: sort_aux (x1 :: xs)
  in
  if is_sorted list then list
  else list |> sort_aux |> sort



let rec naloga1 list =
  let rec aux m = function
    | [] -> m
    | x :: xs -> aux (max m (seat_id x)) xs
  in
  aux 0 list

let naloga2 list =
  let ids = List.map seat_id list in
  let sorted_ids = sort ids in
  let rec aux list =
    match list with
    | x1 :: x2 :: xs ->
      if x1 + 2 = x2 then x2 - 1
      else aux (x2 :: xs)
    | _ -> failwith "Ni delovalo."
  in
  aux sorted_ids



let _ =

  let preberi_datoteko ime_datoteke =
    let chan = open_in ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina
  and izpisi_datoteko ime_datoteke vsebina =
    let chan = open_out ime_datoteke in
    output_string chan vsebina;
    close_out chan
  in

  let vsebina_datoteke =
    (("/home/davidcadez/fmf/prog1/advent_of_code_2020/in/day_" ^ day ^ ".in")
     |> preberi_datoteko |> String.split_on_char '\n' |> clean)
  in

  let time1 = Unix.gettimeofday () in
  let odgovor1 = naloga1 vsebina_datoteke in
  let time_used1 = Unix.gettimeofday () -. time1 in

  let time2 = Unix.gettimeofday () in
  let odgovor2 = naloga2 vsebina_datoteke in
  let time_used2 = Unix.gettimeofday () -. time2 in

  izpisi_datoteko ("/home/davidcadez/fmf/prog1/advent_of_code_2020/out/day_" ^ day ^ "_1.out") ((string_of_int odgovor1) ^ " in " ^ (string_of_float time_used1) ^ "s");
  izpisi_datoteko ("/home/davidcadez/fmf/prog1/advent_of_code_2020/out/day_" ^ day ^ "_2.out") ((string_of_int odgovor2) ^ " in " ^ (string_of_float time_used2) ^ "s")

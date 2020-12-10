#load "unix.cma";;

let day = "10"


let rec aux1 (acc1, acc2) = function
  | [] | [_] -> acc1 * (acc2 + 1)
  | x1 :: x2 :: xs ->
    if x2 = x1 + 1 then aux1 (acc1 + 1, acc2) (x2 :: xs)
    else aux1 (acc1, acc2 + 1) (x2 :: xs)

let rec aux2' l = function
  | [] -> []
  | x :: [] -> [(x :: l)]
  | x1 :: x2 :: xs ->
    if x2 - x1 = 3 then (x1 :: l) :: aux2' [] (x2 :: xs)
    else aux2' (x1 :: l) (x2 :: xs)

let rec aux2 = function
  | x1 :: x2 :: x3 :: x4 :: xs ->
    if x1 - x4 = 3 then
      aux2 (x2 :: x3 :: x4 :: xs) +
      aux2 (x3 :: x4 :: xs) +
      aux2 (x4 :: xs) else
    if x1 - x3 = 3 then
      aux2 (x2 :: x3 :: x4 :: xs) +
      aux2 (x3 :: x4 :: xs)
    else aux2 (x2 :: x3 :: x4 :: xs)
  | x1 :: x2 :: x3 :: [] -> if x1 - x3 < 4 then 2 else 1
  | _ -> 1


let naloga1 list =
  list |> List.sort (fun x y -> if x < y then -1 else 1) |> aux1 (1, 0)

let naloga2 list =
  list |> List.sort (fun x y -> if x < y then -1 else 1)
  |> aux2' [0] |> List.map aux2 |> List.fold_left ( * ) 1


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
     |> preberi_datoteko |> String.split_on_char '\n' |> List.filter (fun x -> not (x = "")) |> List.map int_of_string)
  in

  let time1 = Unix.gettimeofday () in
  let odgovor1 = naloga1 vsebina_datoteke in
  let time_used1 = Unix.gettimeofday () -. time1 in

  let time2 = Unix.gettimeofday () in
  let odgovor2 = naloga2 vsebina_datoteke in
  let time_used2 = Unix.gettimeofday () -. time2 in

  izpisi_datoteko ("/home/davidcadez/fmf/prog1/advent_of_code_2020/out/day_" ^ day ^ "_1.out") ((string_of_int odgovor1) ^ " in " ^ (string_of_float time_used1) ^ "s");
  izpisi_datoteko ("/home/davidcadez/fmf/prog1/advent_of_code_2020/out/day_" ^ day ^ "_2.out") ((string_of_int odgovor2) ^ " in " ^ (string_of_float time_used2) ^ "s")

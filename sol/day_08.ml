#load "unix.cma";;

let day = "08"

let rec clean i = function
  | [] -> []
  | "" :: xs -> clean i xs
  | x :: xs ->
    let name :: n :: [] = String.split_on_char ' ' x in
    (i, [name; n]) :: clean (i + 1) xs


let rec aux1 l1 l2 acc i =
  let [name; n] = (List.assoc i l1) in
  if List.mem i l2 then acc else
    match name with
    | "acc" -> aux1 l1 (i :: l2) (acc + int_of_string n) (i + 1)
    | "nop" -> aux1 l1 (i :: l2) acc (i + 1)
    | "jmp" -> aux1 l1 (i :: l2) acc (i + int_of_string n)

let rec aux2 l1 l2 acc i =
  if i > 628 then (acc, true) else
  if List.mem i l2 then (acc, false) else
    match (List.assoc i l1) with
    | ["acc"; n] -> aux2 l1 (i :: l2) (acc + int_of_string n) (i + 1)
    | ["nop"; n] -> aux2 l1 (i :: l2) acc (i + 1)
    | ["jmp"; n] -> aux2 l1 (i :: l2) acc (i + int_of_string n)

let naloga1 list = aux1 list [] 0 0

let rec change i = function
  | [] -> []
  | (k, ["jmp"; n]) :: xs ->
    if k = i then (i, ["nop"; n]) :: change i xs
    else (k, ["jmp"; n]) :: change i xs
  | (k, ["nop"; n]) :: xs ->
    if k = i then (i, ["jmp"; n]) :: change i xs
    else (k, ["nop"; n]) :: change i xs
  | x :: xs -> x :: change i xs

let rec naloga2 i list =
  if i > 700 then failwith "Unexpected argument in naloga2." else
    let (a, b) = aux2 (change i list) [] 0 0 in
    if b then a else naloga2 (i + 1) list

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
     |> preberi_datoteko |> String.split_on_char '\n' |> clean 0)
  in

  let time1 = Unix.gettimeofday () in
  let odgovor1 = naloga1 vsebina_datoteke in
  let time_used1 = Unix.gettimeofday () -. time1 in

  let time2 = Unix.gettimeofday () in
  let odgovor2 = naloga2 0 vsebina_datoteke in
  let time_used2 = Unix.gettimeofday () -. time2 in

  izpisi_datoteko ("/home/davidcadez/fmf/prog1/advent_of_code_2020/out/day_" ^ day ^ "_1.out") ((string_of_int odgovor1) ^ " in " ^ (string_of_float time_used1) ^ "s");
  izpisi_datoteko ("/home/davidcadez/fmf/prog1/advent_of_code_2020/out/day_" ^ day ^ "_2.out") ((string_of_int odgovor2) ^ " in " ^ (string_of_float time_used2) ^ "s")

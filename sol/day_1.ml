let day = "1"

let rec string_list_to_int_list = function
  | [] -> []
  | x :: xs ->
  if x = "" then 0 :: (string_list_to_int_list xs)
  else (int_of_string x) :: (string_list_to_int_list xs)

let rec naloga1 vsebina_datoteke =
  match vsebina_datoteke with
  | [] -> failwith "Ne obstaja."
  | x :: xs ->
  if x = "" then naloga1 xs else
    let x1, xs1 = int_of_string x, string_list_to_int_list xs in
    if List.mem (2020 - x1) xs1
    then x1 * (2020 - x1)
    else naloga1 xs

let rec naloga2 vsebina_datoteke =
  let rec aux k list =
    match list with
    | [] -> 0
    | x :: xs ->
      if List.mem (k - x) xs
      then x * (k - x)
      else aux k xs
  in
  match vsebina_datoteke with
  | [] -> failwith "Ne obstaja."
  | x :: xs ->
  if x = "" then naloga2 xs else
    let x1, xs1 = int_of_string x, string_list_to_int_list xs in
    let t = aux (2020 - x1) xs1 in
    if t = 0
    then naloga2 xs
    else x1 * t

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
    (("/home/davidcadez/FMF/PROG1/advent_of_code_2020/in/day_" ^ day ^ ".in")
    |> preberi_datoteko |> String.split_on_char '\n')
  in

  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in

  izpisi_datoteko ("/home/davidcadez/FMF/PROG1/advent_of_code_2020/out/day_" ^ day ^ "_1.out") (string_of_int odgovor1);
  izpisi_datoteko ("/home/davidcadez/FMF/PROG1/advent_of_code_2020/out/day_" ^ day ^ "_2.out") (string_of_int odgovor2)

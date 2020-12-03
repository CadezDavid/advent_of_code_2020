let day = "3"

let naloga1 list =
  let rec aux acc i list =
    match list with
    | [] -> acc
    | x :: xs ->
      if x = "" then aux acc ((i + 3) mod 31) xs else
      if x.[i] = '#' then aux (1 + acc) ((i + 3) mod 31) xs
      else aux acc ((i + 3) mod 31) xs
  in
  aux 0 0 list

let naloga2 list =
  let rec aux2 acc i list line a b =
    match list with
    | [] | [""] -> acc
    | x :: xs ->
      if line mod b = 0 then
        if (x.[i] = '#') then aux2 (1 + acc) ((i + a) mod 31) xs (line + 1) a b
        else aux2 acc ((i + a) mod 31) xs (line + 1) a b
      else aux2 acc i xs (line + 1) a b
  in
  let list_of_pairs = [(1, 1); (3, 1); (5, 1); (7, 1); (1, 2)] in
  let rec aux2' list1 list2 =
    match list2 with
    | [] -> 1
    | (a, b) :: xs ->
      print_int (aux2 0 0 list1 0 a b);
      print_char '\n';
      (aux2 0 0 list1 0 a b) * (aux2' list1 xs)
  in
  aux2' list list_of_pairs

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
     |> preberi_datoteko |> String.split_on_char '\n')
  in

  let odgovor1 = naloga1 vsebina_datoteke in

  let odgovor2 = naloga2 vsebina_datoteke in

  izpisi_datoteko ("/home/davidcadez/fmf/prog1/advent_of_code_2020/out/day_" ^ day ^ "_1.out") (string_of_int odgovor1);
  izpisi_datoteko ("/home/davidcadez/fmf/prog1/advent_of_code_2020/out/day_" ^ day ^ "_2.out") (string_of_int odgovor2)

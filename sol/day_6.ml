#load "unix.cma"

let day = "6"



let rec union s1 = function
  | "" -> ""
  | s2 ->
    if String.contains s1 s2.[0]
    then union s1 (String.sub s2 1 ((String.length s2) - 1))
    else union (s1 ^ (String.make 1 s2.[0])) (String.sub s2 1 ((String.length s2) - 1))

let rec intersection s1 = function
  | "" -> ""
  | s2 ->
    if String.contains s1 s2.[0]
    then (String.make 1 s2.[0]) ^ intersection s1 (String.sub s2 1 ((String.length s2) - 1))
    else intersection s1 (String.sub s2 1 ((String.length s2) - 1))



let naloga1 list =
  let rec aux acc current list =
    match list with
    | [] -> acc + String.length current
    | "" :: xs -> aux (acc + String.length current) "" xs
    | x :: xs -> aux acc (union x current) xs
  in
  aux 0 "" list

let naloga2 list =
  let rec aux acc current list =
    match list with
    | [] -> acc + String.length current
    | "" :: xs -> aux (acc + String.length current) "abcdefghijklmnoprstuvzxyqw" xs
    | x :: xs -> aux acc (intersection x current) xs
  in
  aux 0 "abcdefghijklmnoprstuvzxyqw" list



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

  let time1 = Unix.gettimeofday () in
  let odgovor1 = naloga1 vsebina_datoteke in
  let time_used1 = Unix.gettimeofday () -. time1 in

  let time2 = Unix.gettimeofday () in
  let odgovor2 = naloga2 vsebina_datoteke in
  let time_used2 = Unix.gettimeofday () -. time2 in

  izpisi_datoteko ("/home/davidcadez/fmf/prog1/advent_of_code_2020/out/day_" ^ day ^ "_1.out") ((string_of_int odgovor1) ^ " in " ^ (string_of_float time_used1) ^ "s");
  izpisi_datoteko ("/home/davidcadez/fmf/prog1/advent_of_code_2020/out/day_" ^ day ^ "_2.out") ((string_of_int odgovor2) ^ " in " ^ (string_of_float time_used2) ^ "s")

#load "unix.cma"

let day = "06"
let alphabet = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z']

let list_of_string s = List.init (String.length s) (String.get s)

let rec union l = function
  | [] -> l
  | x :: xs -> if List.mem x l then union l xs else x :: union l xs

let rec intersection l = function
  | [] -> []
  | x :: xs -> if List.mem x l then x :: intersection l xs else intersection l xs


let naloga1 list =
  let rec aux acc current = function
    | [] -> acc + (List.length current)
    | "" :: xs -> aux (acc + List.length current) [] xs
    | x :: xs -> aux acc (union (list_of_string x) current) xs
  in
  aux 0 [] list

let naloga2 list =
  let rec aux acc current = function
    | [] | [""] -> acc + (List.length current)
    | "" :: xs -> aux (acc + List.length current) alphabet xs
    | x :: xs -> aux acc (intersection (list_of_string x) current) xs
  in
  aux 0 alphabet list


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

let day = "2"


let rec string_to_list string =
  string |> String.split_on_char ' '

let rec count char string =
  let rec aux acc i max =
    if i = max then acc else
    if string.[i] = char then aux (1 + acc) (1 + i) max else aux acc (1 + i) max
  in
  aux 0 0 (String.length string)


let rec valid_password1 list =
  match list with
  | x :: c :: password :: [] ->
    let a :: b :: [] = String.split_on_char '-' x in
    let min = int_of_string a in
    let max = int_of_string b in
    let char = c.[0] in
    let k = (count char password) in
    if (k <= max) && (min <= k) then 1 else 0
  | _ -> 0

let rec valid_password2 list =
  match list with
  | x :: c :: password :: [] ->
    let a :: b :: [] = String.split_on_char '-' x in
    let min = int_of_string a in
    let max = int_of_string b in
    let char = c.[0] in
    let first = (password.[min - 1] = char) in
    let second = (password.[max - 1] = char) in
    if (first || second) && ((not first) || (not second)) then 1 else 0
  | _ -> 0


let rec naloga1 = function
  | [] -> 0
  | x :: xs -> (valid_password1 (string_to_list x)) + (naloga1 xs)

let rec naloga2 = function
  | [] -> 0
  | x :: xs -> (valid_password2 (string_to_list x)) + (naloga2 xs)


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

  let start_time1 = Sys.time() in
  let odgovor1 = naloga1 vsebina_datoteke in
  let time_used1 = Sys.time() -. start_time1 in

  let start_time2 = Sys.time() in
  let odgovor2 = naloga2 vsebina_datoteke in
  let time_used2 = Sys.time() -. start_time2 in

  izpisi_datoteko ("/home/davidcadez/FMF/PROG1/advent_of_code_2020/out/day_" ^ day ^ "_1.out") ((string_of_int odgovor1) ^ " in " ^ (string_of_float time_used1) ^ "s");
  izpisi_datoteko ("/home/davidcadez/FMF/PROG1/advent_of_code_2020/out/day_" ^ day ^ "_2.out") ((string_of_int odgovor2) ^ " in " ^ (string_of_float time_used2) ^ "s")

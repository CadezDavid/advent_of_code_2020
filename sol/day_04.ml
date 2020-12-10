#load "unix.cma";;
#load "str.cma"

let day = "04"

let check_byr b =
  let year =
    try Some (int_of_string b)
    with Failure _ -> None
  in
  match year with
  | None -> 0
  | Some x -> if 1920 <= x && x <= 2002 then 1 else 0

let check_iyr b =
  let year =
    try Some (int_of_string b)
    with Failure _ -> None
  in
  match year with
  | None -> 0
  | Some x -> if 2010 <= x && x <= 2020 then 1 else 0

let check_eyr b =
  let year =
    try Some (int_of_string b)
    with Failure _ -> None
  in
  match year with
  | None -> 0
  | Some x -> if (2020 <= x) && (x <= 2030) then 1 else 0

let check_hgt b =
  let r = Str.regexp "^[0-9][0-9][0-9]?..$" in
  let t = try Some (Str.search_forward r b 0) with Not_found -> None in
  match t with
  | Some _ ->
    (let height = int_of_string (Str.string_before b ((String.length b) - 2)) in
     match Str.last_chars b 2 with
     | "in" -> if 59 <= height && height <= 76 then 1 else 0
     | "cm" -> if 150 <= height && height <= 193 then 1 else 0
     | _ -> 0)
  | None -> 0

let check_hcl b =
  let r = Str.regexp "^#[a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9]$" in
  let t = try Some (Str.search_forward r b 0) with Not_found -> None in
  match t with
  | Some _ -> 1
  | None -> 0

let check_ecl b =
  if List.mem b ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]
  then 1
  else 0

let check_pid b =
  if String.length b = 9 then
    let t = try Some (int_of_string b) with Failure _ -> None in
    match t with
    | Some _ -> 1
    | None -> 0
  else 0



let valid1 list =
  let rec fields = function
    | [] -> 0
    | x :: xs ->
      match String.split_on_char ':' x with
      | ["cid"; _] -> fields xs
      | [_ ; _] -> 1 + fields xs
      | _ -> failwith "Unexpected argument."
  in
  if fields list = 7 then true else false

let valid2 list =
  let valid_input string =
    match String.split_on_char ':' string with
    | ["byr"; b] -> check_byr b
    | ["iyr"; b] -> check_iyr b
    | ["eyr"; b] -> check_eyr b
    | ["hgt"; b] -> check_hgt b
    | ["hcl"; b] -> check_hcl b
    | ["ecl"; b] -> check_ecl b
    | ["pid"; b] -> check_pid b
    | [_; _] -> 0
    | _ -> failwith "Unexpected argument."
  in
  let rec fields list =
    match list with
    | [] -> 0
    | x :: xs -> (valid_input x) + fields xs
  in
  let sol = fields list in
  if sol = 7 then true else false



let naloga1 list =
  let rec make_passports count passport = function
    | [] -> count
    | "" :: xs ->
      if valid1 passport
      then make_passports (1 + count) [] xs
      else make_passports count [] xs
    | x :: xs -> make_passports count ((String.split_on_char ' ' x) @ passport) xs
  in
  make_passports 0 [] list

let naloga2 list =
  let rec make_passports count passport = function
    | [] ->
      if valid2 passport
      then 1 + count
      else count
    | "" :: xs ->
      if valid2 passport
      then make_passports (1 + count) [] xs
      else make_passports count [] xs
    | x :: xs -> make_passports count ((String.split_on_char ' ' x) @ passport) xs
  in
  make_passports 0 [] list



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

#load "unix.cma"
let day = "7"

let rec clean = function
  | [] -> []
  | "" :: xs -> clean xs
  | x :: xs -> x :: clean xs

let tuple_of_string s =
  let first1 :: first2 :: bags :: contain :: xs = String.split_on_char ' ' s in
  let rec other = function
    | n :: other1 :: other2 :: bags :: xs -> (int_of_string n, other1 ^ " " ^ other2) :: other xs
    | _ -> []
  in
  (first1 ^ " " ^ first2, other xs)


let rec aux1 b l1 l2 l3 =
  match l3 with
  | [] -> if b then aux1 false l1 [] l2 else List.length (l1)
  | (first, other) :: xs ->
    let rec aux1' l1 = function
      | [] -> false
      | (n, name) :: xs -> if List.mem_assoc name l1 then true else aux1' l1 xs
    in
    if aux1' l1 other
    then aux1 true ((first, other) :: l1) l2 xs
    else aux1 b l1 ((first, other) :: l2) xs

let rec aux2 l1 l2 first =
  let a = try List.assoc first l2 with Not_found -> 0 in
  if a > 0 then a else
    let other = List.assoc first l1 in
    if other = [] then 1 else
      let rec aux2' l1 l2 = function
        | [] -> 0
        | (n, name) :: xs ->
          let current = aux2 l1 l2 name in
          n * current + aux2' l1 ((name, current) :: l2) xs
      in
      1 + aux2' l1 l2 other


let naloga1 list =
  aux1 false [("shiny gold", ([]))] [] list - 1

let naloga2 list =
  aux2 list [] "shiny gold" - 1


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
     |> preberi_datoteko |> String.split_on_char '\n' |> clean |> List.map tuple_of_string)
  in

  let time1 = Unix.gettimeofday () in
  let odgovor1 = naloga1 vsebina_datoteke in
  let time_used1 = Unix.gettimeofday () -. time1 in

  let time2 = Unix.gettimeofday () in
  let odgovor2 = naloga2 vsebina_datoteke in
  let time_used2 = Unix.gettimeofday () -. time2 in

  izpisi_datoteko ("/home/davidcadez/fmf/prog1/advent_of_code_2020/out/day_" ^ day ^ "_1.out") ((string_of_int odgovor1) ^ " in " ^ (string_of_float time_used1) ^ "s");
  izpisi_datoteko ("/home/davidcadez/fmf/prog1/advent_of_code_2020/out/day_" ^ day ^ "_2.out") ((string_of_int odgovor2) ^ " in " ^ (string_of_float time_used2) ^ "s")





(* let tuple_of_string s =
   let first =
    match String.split_on_char ' ' s with
    | first1 :: first2 :: _  -> first1 ^ " " ^ first2
    | _ -> failwith "Unexpected argument in tuple_of_string."
   in
   let r = Str.regexp "[0-9] [a-zA-Z]+ [a-zA-Z]+ bag" in
   let rec other s =
    let t = try (Str.search_forward r s 0) with Not_found -> -1 in
    if t = -1 then [] else
      match String.split_on_char ' ' (Str.matched_string s) with
      | n :: name1 :: name2 :: ["bag"] -> (int_of_string n, name1 ^ " " ^ name2) :: other (Str.replace_first r "" s)
      | _ -> failwith "Unexpected argument in tuple_of_string."
   in
   (first, other s) *)

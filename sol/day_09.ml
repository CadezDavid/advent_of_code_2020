#load "unix.cma";;

let day = "09"

(* Tole sem si pa sposodil s strani *)
(* http://createsoftware.users.sourceforge.net/articles/Sorting%20in%20OCaml%20-%20C.%20Pit--Claudel.pdf *)
let quicksort list =
  let split list pivot =
    let rec split_aux inf sup = function
      | [] -> (inf, sup)
      | x :: xs ->
        if x < pivot then split_aux (x :: inf) sup xs
        else split_aux inf (x :: sup) xs
    in
    split_aux [] [] list
  in
  let rec sort result = function
    | [] -> result
    | [x] -> x :: result
    | pivot :: xs ->
      let (inf, sup) = split xs pivot in
      sort (pivot :: (sort result inf)) sup
  in
  List.rev (sort [] list)

let rec first_rest i list =
  match (i, list) with
  | (0, _) -> ([], list)
  | (_, x :: xs) ->
    let (f, r) = first_rest (i - 1) xs in
    (x :: f, r)
  | (_, []) -> (list, [])

let rec my_mem n = function
  | [] -> false
  | x :: xs ->
    if n = x then true else
    if n < x then false else
      my_mem n xs

let rec aux1' n = function
  | [] -> false
  | x :: xs ->
    if (n/2) < x then false else
    if my_mem (n - x) xs then true
    else aux1' n xs

let rec aux1 curr n rest =
  if aux1' n (quicksort curr) then
    let r :: rs = rest in
    let c :: cs = curr in
    aux1 ((List.tl curr) @ [n]) r rs
  else n

let rec naloga1 list =
  let (first, r :: rs) = first_rest 25 list in
  aux1 first r rs

let rec my_max = function
  | [x] -> x
  | x1 :: x2 :: xs -> my_max ((max x1 x2) :: xs)
  | [] -> failwith "Unexpected argument in my_max."

let rec my_min = function
  | [x] -> x
  | x1 :: x2 :: xs -> my_min ((min x1 x2) :: xs)
  | [] -> failwith "Unexpected argument in my_min."

let rec aux2 n acc (curr1, curr2) rest =
  if acc = n then my_min (curr1 @ curr2) + my_max (curr1 @ curr2) else
    let r :: rs = rest in
    if acc < n then aux2 n (acc + r) (curr1, (r :: curr2)) rs else
    if curr1 = [] then aux2 n acc ((List.rev (List.tl curr2)), [List.hd curr2]) rest else
      let c :: cs = curr1 in
      aux2 n (acc - c) (cs, curr2) rest

let naloga2 n list =
  aux2 n 0 ([], []) list


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
  let odgovor2 = naloga2 odgovor1 vsebina_datoteke in
  let time_used2 = Unix.gettimeofday () -. time2 in

  izpisi_datoteko ("/home/davidcadez/fmf/prog1/advent_of_code_2020/out/day_" ^ day ^ "_1.out") ((string_of_int odgovor1) ^ " in " ^ (string_of_float time_used1) ^ "s");
  izpisi_datoteko ("/home/davidcadez/fmf/prog1/advent_of_code_2020/out/day_" ^ day ^ "_2.out") ((string_of_int odgovor2) ^ " in " ^ (string_of_float time_used2) ^ "s")

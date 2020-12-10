#load "unix.cma";;

let day = "10"

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


let rec aux1 (acc1, acc2) = function
  | [x] -> acc1 * (acc2 + 1)
  | x1 :: x2 :: xs ->
    if x2 = x1 + 1 then aux1 (acc1 + 1, acc2) (x2 :: xs)
    else aux1 (acc1, acc2 + 1) (x2 :: xs)
  | _ -> failwith "Unexpected argument in aux1."

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
  aux1 (1, 0) (quicksort list)

let naloga2 list =
  let rec mul f = function
    | [] -> 1
    | x :: xs -> (f x) * mul f xs
  in
  mul aux2 (aux2' [] (0 :: quicksort list))


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

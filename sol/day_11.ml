#load "unix.cma";;

let day = "11"



let rec aux1' l (x, y) ((dx, dy) :: ds) =
  let m = try List.assoc (x + dx, y + dy) l with Not_found -> None in
  match m with
  | None | Some false -> if ds = [] then 0 else aux1' l (x, y) ds
  | Some true -> if ds = [] then 1 else 1 + aux1' l (x, y) ds


let rec format_lines y list =
  print_string "Pretvarjam";
  print_int y;
  print_string "vrstico\n";
  let rec aux s x m =
    if x > m then [] else
    if s.[x] = '.'
    then ((x, y), None) :: aux s (x + 1) m
    else ((x, y), Some true) :: aux s (x + 1) m
  in
  match list with
  | [] -> []
  | "" :: xs -> format_lines y xs
  | x :: xs -> (aux x 0 ((String.length x) - 1)) @ (format_lines (y + 1) xs)


let rec aux1 l l1 l2 =
  let rec aux l ((x, y), Some b) =
    let curr =  aux1' l (x, y) [(1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); (1, -1)] in
    if curr = 0 then ((x, y), Some true) else
    if curr >= 4 then ((x, y), Some false) else
      ((x, y), Some b)
  in
  match l2 with
  | [] -> l1
  | ((x, y), Some b) :: xs -> aux1 l ((aux l ((x, y), Some b)) :: l1) xs
  | ((x, y), None) :: xs -> aux1 l (((x, y), None) :: l1) xs

let rec sum = function
  | [] -> 0
  | ((_, _), Some true) :: xs -> 1 + sum xs
  | _ :: xs -> sum xs

let rec same l1 = function
  | [] -> true
  | ((x, y), b) :: xs -> if List.assoc (x, y) l1 = b then same l1 xs else false

let rec cycle l =
  print_string "Zdej delam nov krog.\n";
  let nov = aux1 l [] l in
  if same nov l then sum l else (cycle nov)

let rec naloga1 list =
  let seats = format_lines 0 list in
  print_string "Zdej grem v cycle.\n";
  cycle seats








let rec neka_funkcija list (x, y) (dx, dy) =
  let nekej = try List.assoc (x, y) list with Not_found -> Some false in
  if nekej = None then neka_funkcija list (x + dx, y + dy) (dx, dy)
  else (x, y)

let rec aux2' list (x, y) = function
  | [] -> []
  | (dx, dy) :: xs -> (neka_funkcija list (x + dx, y + dy) (dx, dy)) :: aux2' list (x, y) xs


let rec what_can_he_see list = function
  | [] -> []
  | ((x, y), z) :: xs ->
    let what_he_sees = aux2' list (x, y) [(1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); (1, -1)] in
    ((x, y), (z, what_he_sees)) :: what_can_he_see list xs

let rec sum' = function
  | [] -> 0
  | ((_, _), (Some true, _)) :: xs -> 1 + sum' xs
  | _ :: xs -> sum' xs

let rec same2 l1 = function
  | [] -> true
  | ((x, y), (b, c)) :: xs -> if List.assoc (x, y) l1 = (b, c) then same2 l1 xs else false

let rec aux42 l1 = function
  | [] -> 0
  | (x, y) :: xs ->
    let (nekej, _) = try List.assoc (x, y) l1 with Not_found -> (None, []) in
    if nekej = Some true then 1 + aux42 l1 xs else aux42 l1 xs


let rec aux2 b l1 l2 = function
  | [] -> (b, l2)
  | ((x, y), (Some c, what_he_sees)) :: xs ->
    (let curr = aux42 l1 what_he_sees in

     if curr = 0 then
       if c = true then aux2 b l1 (((x, y), (Some c, what_he_sees)) :: l2) xs
       else aux2 true l1 (((x, y), (Some true, what_he_sees)) :: l2) xs
     else

     if curr >= 5 then
       if c = false then aux2 b l1 (((x, y), (Some c, what_he_sees)) :: l2) xs
       else aux2 true l1 (((x, y), (Some false, what_he_sees)) :: l2) xs
     else

       aux2 b l1 (((x, y), (Some c, what_he_sees)) :: l2) xs)
  | x :: xs -> aux2 b l1 (x :: l2) xs

let rec cycle' list =
  let (b, l) = aux2 false list [] list in
  if not b then sum' l else cycle' l

let rec naloga2 list =
  let seats = what_can_he_see (format_lines 0 list) (format_lines 0 list) in
  cycle' seats


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

  (* let time1 = Unix.gettimeofday () in
     let odgovor1 = naloga1 vsebina_datoteke in
     let time_used1 = Unix.gettimeofday () -. time1 in *)

  let time2 = Unix.gettimeofday () in
  let odgovor2 = naloga2 vsebina_datoteke in
  let time_used2 = Unix.gettimeofday () -. time2 in

  (* izpisi_datoteko ("/home/davidcadez/fmf/prog1/advent_of_code_2020/out/day_" ^ day ^ "_1.out") ((string_of_int odgovor1) ^ " in " ^ (string_of_float time_used1) ^ "s"); *)
  izpisi_datoteko ("/home/davidcadez/fmf/prog1/advent_of_code_2020/out/day_" ^ day ^ "_2.out") ((string_of_int odgovor2) ^ " in " ^ (string_of_float time_used2) ^ "s")

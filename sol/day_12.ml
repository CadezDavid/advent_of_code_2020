#load "unix.cma";;

let day = "12"

let rec my_tuple_of_string s =
  let d = int_of_string (String.sub s 1 ((String.length s) - 1)) in
  if s.[0] = 'R' then ('L', -d) else
    (s.[0], d)

let rec naloga1 (x, y) curr = function
  | [] -> (abs x) + (abs y)
  | ('N', dis) :: xs -> naloga1 (x, y + dis) curr xs
  | ('S', dis) :: xs -> naloga1 (x, y - dis) curr xs
  | ('W', dis) :: xs -> naloga1 (x - dis, y) curr xs
  | ('E', dis) :: xs -> naloga1 (x + dis, y) curr xs
  | ('L', dis) :: xs ->
    (match curr, ((dis / 90) mod 4 + 4) mod 4 with
     | 'N', 3 | 'E', 0 | 'S', 1 | 'W', 2 -> naloga1 (x, y) 'E' xs
     | 'N', 2 | 'E', 3 | 'S', 0 | 'W', 1 -> naloga1 (x, y) 'S' xs
     | 'N', 1 | 'E', 2 | 'S', 3 | 'W', 0 -> naloga1 (x, y) 'W' xs
     | 'N', 0 | 'E', 1 | 'S', 2 | 'W', 3 -> naloga1 (x, y) 'N' xs)
  | ('F', dis) :: xs ->
    (match curr with
     | 'N' -> naloga1 (x, y + dis) 'N' xs
     | 'S' -> naloga1 (x, y - dis) 'S' xs
     | 'W' -> naloga1 (x - dis, y) 'W' xs
     | 'E' -> naloga1 (x + dis, y) 'E' xs)


let rec naloga2 (x, y) (x1, y1) list =
  print_int x;
  print_string "  ";
  print_int y;
  print_string "\n";
  match list with
  | [] -> (abs x) + (abs y)
  | ('N', dis) :: xs -> naloga2 (x, y) (x1, y1 + dis) xs
  | ('S', dis) :: xs -> naloga2 (x, y) (x1, y1 - dis) xs
  | ('W', dis) :: xs -> naloga2 (x, y) (x1 - dis, y1) xs
  | ('E', dis) :: xs -> naloga2 (x, y) (x1 + dis, y1) xs
  | ('L', dis) :: xs ->
    (match ((dis / 90) mod 4 + 4) mod 4 with
     | 0 -> naloga2 (x, y) (x1, y1) xs
     | 1 -> naloga2 (x, y) (-y1, x1) xs
     | 2 -> naloga2 (x, y) (-x1, -y1) xs
     | 3 -> naloga2 (x, y) (y1, -x1) xs)
  | ('F', dis) :: xs ->
    naloga2 (x + dis * x1, y + dis * y1) (x1, y1) xs


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
     |> preberi_datoteko |> String.split_on_char '\n' |> List.filter (fun x -> not (x = "")) |> List.map my_tuple_of_string)
  in

  let time1 = Unix.gettimeofday () in
  let odgovor1 = naloga1 (0, 0) 'E' vsebina_datoteke in
  let time_used1 = Unix.gettimeofday () -. time1 in

  let time2 = Unix.gettimeofday () in
  let odgovor2 = naloga2 (0, 0) (10, 1) vsebina_datoteke in
  let time_used2 = Unix.gettimeofday () -. time2 in

  izpisi_datoteko ("/home/davidcadez/fmf/prog1/advent_of_code_2020/out/day_" ^ day ^ "_1.out") ((string_of_int odgovor1) ^ " in " ^ (string_of_float time_used1) ^ "s");
  izpisi_datoteko ("/home/davidcadez/fmf/prog1/advent_of_code_2020/out/day_" ^ day ^ "_2.out") ((string_of_int odgovor2) ^ " in " ^ (string_of_float time_used2) ^ "s")

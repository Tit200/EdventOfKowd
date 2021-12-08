let preberi_datoteko ime_datoteke =
  let chan = open_in ime_datoteke in
  let vsebina = really_input_string chan (in_channel_length chan) in
  close_in chan;
  vsebina

let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan

module List = struct
  include List

  let int_list l = List.map int_of_string l

  let sum l =
    let rec sum' a = function [] -> a | x :: xs -> sum' (a + x) xs in
    sum' 0 l

  let lines = String.split_on_char '\n'
end

module type Solver = sig
  val naloga1 : string -> string

  val naloga2 : string -> string -> string
end

(* Tukaj re-definirajte funkcijo naloga1 in naloga2 *)
module Solver7 : Solver = struct

  let find_max list = List.fold_left max min_int list

  let find_min list = List.fold_left min max_int list


  let sum_fuel list =
    let rec sum_fuel_aux b l acc fuel max_value = match l with
      | [] -> if b <= max_value then sum_fuel_aux (b + 1) list (fuel::acc) 0 max_value else acc
      | x::xs -> sum_fuel_aux b xs acc (fuel + (abs (b - x))) max_value
  in
  sum_fuel_aux 0 list [] 0 (find_max list)

  let naloga1 data =   
    let lines = String.split_on_char ',' data in 
    lines |> List.map (int_of_string) |> sum_fuel |> find_min |> string_of_int

  let sum_fuel_crap list =
    let rec sum_fuel_crap_aux b l acc fuel max_value = match l with
      | [] -> if b <= max_value then sum_fuel_crap_aux (b + 1) list (fuel::acc) 0 max_value else acc
      | x::xs -> sum_fuel_crap_aux b xs acc (fuel + ((abs (b - x)) * ((abs (b - x)) + 1))/2) max_value
  in
  sum_fuel_crap_aux 0 list [] 0 (find_max list)

  let naloga2 data _part1 = let lines = String.split_on_char ',' data in 
  lines |> List.map (int_of_string) |> sum_fuel_crap |> find_min |> string_of_int
  end
    

(* Poženemo zadevo *)
let choose_solver : string -> (module Solver) = function
  | "7" -> (module Solver7)
  | _ -> failwith "Ni še rešeno"

let main () =
  let day = Sys.argv.(1) in
  print_endline ("Solving DAY: " ^ day);
  let (module Solver) = choose_solver day in
  let input_data = preberi_datoteko ("data/day_" ^ day ^ ".in") in
  let p1_start = Sys.time () in
  let part1 = Solver.naloga1 input_data in
  let t1_time = Sys.time () -. p1_start in
  print_endline "PART 1:";
  print_endline part1;
  print_endline ("Taken: " ^ string_of_float t1_time ^ "s");
  let p2_start = Sys.time () in
  let part2 = Solver.naloga2 input_data part1 in
  let t2_time = Sys.time () -. p2_start in
  print_endline "PART 2:";
  print_endline part2;
  print_endline ("Taken: " ^ string_of_float t2_time ^ "s");
  print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");
  izpisi_datoteko ("out/day_" ^ day ^ "_1.out") part1;
  izpisi_datoteko ("out/day_" ^ day ^ "_2.out") part2;
  ()

let _ = main ()
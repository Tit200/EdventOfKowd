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
module Solver2 : Solver = struct
  let position direction =
    let rec position_aux l acc_d acc_h = match l with
        | [] -> acc_d * acc_h
        | [n;v]::[] ->
          if n = "forward" then position_aux [] acc_d (acc_h + (int_of_string v)) 
          else if n = "down" then position_aux [] (acc_d + (int_of_string v)) acc_h
          else position_aux [] (acc_d - (int_of_string v)) acc_h
        | [n;v]::s -> 
          if n = "forward" then position_aux s acc_d (acc_h + (int_of_string v)) 
          else if n = "down" then position_aux s (acc_d + (int_of_string v)) acc_h
          else position_aux s (acc_d - (int_of_string v)) acc_h   
      in
        position_aux direction 0 0

    let naloga1 data =   
      let lines = String.split_on_char '\n' data in 
      lines |> List.map (String.split_on_char ' ') |> position |> string_of_int

    let new_position direction =
      let rec new_position_aux l acc_d acc_h aim = match l with
            | [] -> acc_d * acc_h
            | [n;v]::[] ->
              if n = "forward" then new_position_aux [] (acc_d + (aim * int_of_string v)) (acc_h + (int_of_string v)) aim 
              else if n = "down" then new_position_aux [] acc_d acc_h (aim + int_of_string v)
              else new_position_aux [] acc_d acc_h (aim - int_of_string v)
            | [n;v]::s -> 
              if n = "forward" then new_position_aux s (acc_d + (aim * int_of_string v)) (acc_h + (int_of_string v)) aim
              else if n = "down" then new_position_aux s acc_d acc_h (aim + int_of_string v)
              else new_position_aux s acc_d acc_h (aim - int_of_string v)   
          in
            new_position_aux direction 0 0 0
    
        let naloga2 data _part1 =   
          let lines = String.split_on_char '\n' data in 
          lines |> List.map (String.split_on_char ' ') |> new_position |> string_of_int
    
    end

(* Poženemo zadevo *)
let choose_solver : string -> (module Solver) = function
  | "2" -> (module Solver2)
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
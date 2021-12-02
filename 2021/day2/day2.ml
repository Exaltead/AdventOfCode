open Core

let read file =  
  let splitter x = match String.split_on_chars ~on:[' '] x with
  | command::value::[] -> (command, int_of_string value)
  | _ -> raise (Invalid_argument x) in
  In_channel.read_lines file |> List.map ~f:splitter

type position = { depth: int; forward: int; aim: int}
(* Task 1

type position = { depth: int; forward: int;}

let execute =  
  let conveter current (command, value) = 
    match command with
    | "forward" -> {depth = current.depth; forward = current.forward + value}
    | "down" -> {depth = current.depth + value; forward = current.forward}
    | "up" -> {depth = current.depth - value; forward = current.forward}
    | _ -> raise (Invalid_argument command)  in
  List.fold ~init:{depth = 0; forward = 0} ~f:conveter
*)

let execute =  
  let conveter current (command, value) = 
    match command with
    | "forward" -> {current with depth = current.depth + value * current.aim; forward = current.forward + value}
    | "down" -> {current with aim = current.aim + value}
    | "up" -> {current with aim = current.aim - value}
    | _ -> raise (Invalid_argument command)  in
  List.fold ~init:{depth = 0; forward = 0; aim = 0} ~f:conveter

let numerize position = position.depth * position.forward

let () = read "input1.txt" |> execute |> numerize |> string_of_int |>  print_endline

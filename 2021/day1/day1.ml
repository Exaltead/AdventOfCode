open Core


let read file = In_channel.read_lines file |>  List.map ~f:int_of_string;;

let rec window samples = 
  match samples with
  | f::s::t::tail -> (f + s + t) :: window(s::t::tail) 
  | _ -> []

let count samples =
  match samples with
  | head :: tail ->  let (_, counter) = List.fold ~init:(head, 0) ~f:(fun (prev, counter) item -> (item, if prev < item then counter + 1 else counter) ) tail in counter
  | _ -> 0



let () =  read "input1.txt" |> window |> count |> string_of_int |> print_endline

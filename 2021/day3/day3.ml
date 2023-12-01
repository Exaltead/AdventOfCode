
(*let read file = Core.In_channel.read_lines file |> List.map  (fun x -> Bytes.of_string x |> (fun t -> Bytes.get_uint8 t 0))*)


let read file = 
  let convert bit_string = 
    let ht = Hashtbl.create 6 in  
    let _ = List.iteri (fun index x -> match x with
    | '1' -> Hashtbl.add ht index (1, 0) 
    | '0' -> Hashtbl.add ht index (0, 1)
    | _ -> raise @@ Invalid_argument "Wrong base") bit_string 
  in ht
in Core.In_channel.read_lines file  |> List.map  (fun x -> String.to_seq x |> List.of_seq |> List.rev |> convert )  ;;
     
 
let compress tables = 
  let merge ht1 ht2 = 
    let _ = Hashtbl.iter (fun k (o1, z1) ->  let (o2, z2) = Hashtbl.find ht1 k in
      Hashtbl.replace ht1 k (o1 + o2, z1 + z2)) ht2 in
      ht1
    in
  List.fold_left  merge  (List.hd tables)  (List.tl tables)


let selector_int table op = Hashtbl.fold (fun key (o, z) current -> 
    if op o  z then (Core.Int.pow 2 key) + current else current) table 0

let result data =
  let gamma = selector_int data (>) in
  let epsilon = selector_int data (<) in
  Format.sprintf "%d %d %d \n" gamma epsilon (gamma * epsilon)


let () =  read "input1.txt" |> compress |> result |>  print_string

let rec take k l = match l with
  | [] -> []
  | _ when k <= 0 -> []
  | x::xs -> x::(take (k-1) xs)

let ngrammes k l =
  if k <= 0 then raise (Invalid_argument "ngrammes");
  let rec aux k l acc =
    match l with
    | [] -> acc
    | _ when k > List.length l -> acc
    | _ -> aux k (List.tl l) ((take k l)::acc)
  in
  List.rev (aux k l [])
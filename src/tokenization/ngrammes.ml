let ngrammes (k: int) (l: 'a list) : 'a list list =
  let rec ngrammes_aux (k: int) (l: 'a list) (acc: 'a list list) : 'a list list =
    match l with
    | [] -> acc
    | _ -> ngrammes_aux k (List.tl l) ((take k l)::acc)
  in
  List.rev (ngrammes_aux k l [])


let rec take k l = match l with
  | [] -> []
  | _ when k <= 0 -> []
  | x::xs -> x::(take (k-1) xs)
open Definitions.MarkovChain

let normalize ed = 
   let MarkovChain t = ed in
   let rec aux l tw = match l with
      | [] -> []
      | hd::tl ->
         let total_weight = match tw with
            | -1 -> List.fold_left (fun acc x -> acc + x.weight) 0 (hd::tl)
            | _ -> tw
         in 
         if total_weight = 0 then {token = hd.token; weight = hd.weight; dest = hd.dest}::(aux tl total_weight)
         else {token = hd.token; weight = int_of_float( (float_of_int hd.weight /. float_of_int total_weight)*.10_000. ); dest = hd.dest}::(aux tl total_weight)
   in for i = 0 to Array.length t - 1 do
      t.(i) <- aux t.(i) (-1)
   done;
   t;;

let rec find_next_token tokens rand =
   match tokens with
   | [] -> failwith "No token found"
   | { token; dest; _ } :: [] -> token,dest
   | { token; weight; dest } :: tl ->
      if rand < weight then token,dest
      else find_next_token tl (rand - weight)

let random_walk ~length ?(start = 0) mc =
   let t = normalize mc in
   let rec aux i current_token = match current_token with
      | [] | _ when i = length -> []
      | _ ->
         try
            let cur_token, next_token = find_next_token current_token (Random.int 10_000) in
            cur_token :: aux (i + 1) t.(next_token)
         with _ -> []
         in aux 0 t.(start);;



random_walk ~length:10 mc_smaller_example
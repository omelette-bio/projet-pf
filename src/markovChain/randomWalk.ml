open Definitions.MarkovChain

(* prend une markov chain et renvoie une markov chain avec le poids des arretes entre 0 et 1, 
   une markov chain est une array de liste de edge hd est une liste d'elements composés des champs token, weight et dest
   *)
let normalize ed = 
   let MarkovChain t = ed in
   let rec aux l tw = match l with
      | [] -> []
      | hd::tl ->
         let total_weight = match tw with
         | -1 -> List.fold_left (fun acc x -> acc + x.weight) 0 (hd::tl)
         | _ -> tw
         in 
         if total_weight = 0 then
            {token = hd.token; weight = hd.weight; dest = hd.dest}::(aux tl total_weight)
         else
            {token = hd.token; weight = int_of_float( (float_of_int hd.weight /. float_of_int total_weight)*.100. ); dest = hd.dest}::(aux tl total_weight)
   in for i = 0 to Array.length t - 1 do
      t.(i) <- aux t.(i) (-1)
   done;
   t;;
(* prend une markov chain et renvoie une markov chain avec le poids des arretes entre 0 et 1*)

let random_walk ~length ?(start = 0) mc =
   (* renvoie la suite de token étiquettant une marche aléatoire de longueur
     `length` au plus; si la marche aléatoire atteint un état
     sans successeurs, on s'arrête et on renvoie la marche obtenue 
   *)
   let t = normalize mc in
   (* print the list *)
   let rec print_list = function 
      [] -> ()
      | e::l -> print_int e.weight ; print_string " " ; print_list l;
   in print_list t.(0);

   (* let rec aux lenght start mc = match 
   in aux length start t *)


   ignore (length, start, mc);
   failwith "TODO";;



random_walk ~length:10 mc_smaller_example
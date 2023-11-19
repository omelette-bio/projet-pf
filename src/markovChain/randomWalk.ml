let random_walk ~length ?(start = 0) mc =
   (* renvoie la suite de token étiquettant une marche aléatoire de longueur
     `length` au plus; si la marche aléatoire atteint un état
     sans successeurs, on s'arrête et on renvoie la marche obtenue 
   *)
   ignore (length, start, mc);
   failwith "TODO"

(*    let rec walk ~length ~start mc =
      if mc.(start) <> [||] then
         (* normalise weight of the elements of markov chain *)
         (* TODO : normalize function *)


let normalize ed =
   let len = Array.length ed in
   let sum = ref 0 in
   for i = 0 to len - 1 do
      sum := !sum + ed.(i).weight
   done;
   for i = 0 to len - 1 do
      ed.(i).weight <- ed.(i).weight / !sum
   done *)
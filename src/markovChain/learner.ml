open Definitions.MarkovChain

let learn_markov_chain ~token_of_arc ~max_state_id ~walks =
  let tab = Array.make (max_state_id+1) [] in
  let rec aux li = match li with
    | [] -> ()
    | hd::tl -> 
      let rec list_iter li = match li with
        | [] | _::[] -> ()
        | s1::s2::tl -> 
          let rec update_state li s1 s2 ~token_of_arc = match li with
            | [] -> [{token = token_of_arc s1 s2; weight = 1; dest = s2}]
            | hd::tl -> if hd.token = (token_of_arc s1 s2) && hd.dest = s2 then {token = hd.token; weight = hd.weight + 1; dest = hd.dest}::tl  
                else hd :: (update_state tl s1 s2 ~token_of_arc)
          in tab.(s1) <- update_state tab.(s1) s1 s2 ~token_of_arc; 
          list_iter (s2::tl)
      in list_iter hd; 
      aux tl
  in aux walks;
  MarkovChain tab
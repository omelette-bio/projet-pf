module Tokenizer = struct

  type vocabulary = char list

  let voc_size voc = List.length voc

  exception EncodingError of string


  let index l e = 
    let rec index_aux l e i = match l with
      | [] -> None
      | hd::tl -> if hd = e then Some i 
                  else (index_aux tl e (i+1))
    in index_aux l e 0
    
  let encode voc s =
    let len = String.length s in
    let encoded = ref [] in
    for i=0 to len-1 do
      match index voc s.[i] with
      | Some j -> encoded := !encoded @ [j]
      (* raise an error with the remaining part of the string *)
      | None -> raise (EncodingError (Printf.sprintf "%s" (String.sub s i (len-i))))
    done;
    !encoded

  exception DecodingError of int

  let decode voc ids =
    let rec decode_aux voc ids decoded = match ids with
      | [] -> decoded
      | hd::tl -> match (List.nth_opt voc hd) with
        | Some c -> decode_aux voc tl (decoded ^ (String.make 1 c))
        | None -> raise (DecodingError hd)
    in decode_aux voc ids ""
 
  let learn batch =
    ignore batch; 
    failwith "todo"

end

module TokenizerCheckType : Definitions.Tokenizer.TOKENIZER = Tokenizer
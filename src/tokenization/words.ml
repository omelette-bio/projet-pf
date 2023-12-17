module Tokenizer = struct

  type vocabulary = (string * int) list

  let voc_size voc = List.length voc

  exception EncodingError of string

  let is_alpha = function 
  | 'a' .. 'z' | 'A' .. 'Z'  -> true 
  | c -> begin
    let accented_characters = 
      "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæ" ^ 
      "çèéêëìíîïðñòóôõö÷øùúûüýþÿĀāĂăĄąĆćĈĉĊċČč" ^
      "ĎďĐđĒēĔĕĖėĘęĚěĜĝĞğĠġĢģĤĥĦħĨĩĪīĬĭĮįİıĲĳĴ" ^
      "ĵĶķĸĹĺĻļĽľĿŀŁłŃńŅņŇňŉŊŋŌōŎŏŐőŒœŔŕŖŗŘřŚś" ^
      "ŜŝŞşŠšŢţŤťŦŧŨũŪūŬŭŮůŰűŲųŴŵŶŷŸŹźŻżŽž"
    in String.contains accented_characters c
  end

  let first_non_alpha_from s start =
    let len = String.length s in
    let rec aux i = 
      if i >= len then len
      else if is_alpha s.[i] then aux (i+1)
      else i
    in aux start

  let alpha_blocks s = 
    let rec aux i l =
      if i >= String.length s then l
      else
        let end_index = first_non_alpha_from s i in
        let len = end_index - i in
        match len with
        | 0 -> begin 
          match List.mem (String.sub s i 1) l with
            | true -> aux (i+1) l
            | false -> aux (i+1) ((String.sub s i 1)::l);
          end
        | _ -> 
          match List.mem (String.sub s i len) l with
            | true -> aux end_index l
            | false -> aux end_index ((String.sub s i len)::l)
      in List.rev(aux 0 [])

  let encode voc s =
    let rec aux s i =
      if i >= String.length s then []
      else 
        let end_index = first_non_alpha_from s i in
        let word = String.sub s i (end_index - i) in
        match String.length word with
          | 0 -> begin
            try (List.assoc (String.sub s i 1) voc)::(aux s (i+1))
            with Not_found -> raise (EncodingError (String.sub s i ((String.length s) - i) ))
          end
          | _ -> begin
            try (List.assoc word voc)::(aux s end_index)
            with Not_found -> raise (EncodingError (String.sub s i ((String.length s) - i) ))
          end
      in aux s 0
          

  exception DecodingError of int

  let find_key voc w = 
    let rec aux = function
      | [] -> None
      | (w', i)::ws -> if i = w then Some w' else aux ws
    in aux voc

  let decode voc ids = 
    let rec decode_aux voc ids decoded = match ids with
    | [] -> decoded
    | hd::tl -> match find_key voc hd with
        | Some c -> decode_aux voc tl (decoded ^ c)
        | None -> raise (DecodingError hd)
  in decode_aux voc ids ""

  
  (* passe les tests *)

  let learn batch =
    let batch2 = alpha_blocks (String.concat " " batch) in
    let rec learn_aux batch voc i =
      match batch with
      | [] -> voc
      | hd :: tl -> learn_aux tl (voc@[(hd,i)]) (i+1)
    in
    learn_aux batch2 [] 0

  (* passe pas les tests mais fonctionne quand même avec des Hashtbl*)
    
   (*  let learn batch =
      let batch2 = alpha_blocks (String.concat " " batch) in
      let voc = Hashtbl.create 10 in
      let rec learn_aux i batch = match batch with
        | [] -> ()
        | hd::tl ->
          match Hashtbl.find_opt voc hd with
          | Some _ -> learn_aux (i) tl;
          | None -> Hashtbl.add voc hd i; learn_aux (i+1) tl;
        in learn_aux 0 batch2;
      List.of_seq(Hashtbl.to_seq voc) *)

end

module TokenizerCheckType : Definitions.Tokenizer.TOKENIZER = Tokenizer
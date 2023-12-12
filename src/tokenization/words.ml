module Tokenizer = struct

  type vocabulary = (string * int) list

  let voc_size voc = List.length voc

  exception EncodingError of string

  module SS = Set.Make(String)

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
    let rec aux start = 
      if start >= String.length s then []
      else 
        let end_index = first_non_alpha_from s start in
        let word = String.sub s start (end_index - start) in
        if String.length word = 0 then String.sub s start 1::(aux (start+1))
        else String.sub s start (String.length word)::(aux end_index)
    in aux 0

  (*  let alpha_blocks s = 
    let blocks = SS.empty in
    let rec aux start set = 
      if start >= String.length s then List.of_seq (SS.to_seq set)
      else
        let end_index = first_non_alpha_from s start in
        let word = String.sub s start (end_index - start) in
        if String.length word = 0 then begin (aux (start+1) (SS.add (String.sub s start 1) set)); end
        else aux end_index (SS.add (String.sub s start (String.length word)) set )
    in aux 0 blocks *)

  let sep_words s =
    let rec aux start =
      if start >= String.length s then []
      else
        let end_index = first_non_alpha_from s start in
        let word = String.sub s start (end_index - start) in
        if String.length word = 0 then (String.sub s start 1)::(aux (start+1))
        else (String.sub s start (String.length word))::(aux end_index)
    in aux 0

  let encode voc s =
    let words = sep_words s in
    let rec aux = function
      | [] -> []
      | w::ws -> 
        begin
          try (List.assoc w voc)::(aux ws)
          with Not_found -> raise (EncodingError (w^(String.concat "" ws)))
        end
    in aux words

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

 (*  let concatenate s =
    let rec aux = function
      | [] -> ""
      | hd::[] -> hd
      | hd::tl -> hd ^ " " ^ (aux tl)
    in aux s *)

  let find_couple voc w =
    let rec aux = function
      | [] -> false
      | (k,_)::tl -> if k = w then true else aux tl
    in aux voc

  (* passe les tests *)

  (* let learn batch =
    let batch2 = sep_words (String.concat " " batch) in
    let rec learn_aux batch voc i =
      match batch with
      | [] -> voc
      | hd :: tl ->
        match find_couple voc hd with
        | true -> learn_aux tl voc i;
        | false -> learn_aux tl (voc@[(hd,i)]) (i + 1)
    in
    learn_aux batch2 [] 0 *)

    (* passe pas les tests mais fonctionne quand même avec des Hashtbl*)
    let learn batch =
      let batch2 = alpha_blocks (String.concat " " batch) in
      let voc = Hashtbl.create 10 in
      let rec learn_aux i batch = match batch with
        | [] -> ()
        | hd::tl ->
          match Hashtbl.find_opt voc hd with
          | Some _ -> learn_aux (i) tl;
          | None -> Hashtbl.add voc hd i; learn_aux (i+1) tl;
        in learn_aux 0 batch2;
      List.of_seq(Hashtbl.to_seq voc)

end

module TokenizerCheckType : Definitions.Tokenizer.TOKENIZER = Tokenizer
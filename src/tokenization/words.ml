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
    let rec aux start = 
      if start >= String.length s then []
      else 
        let end_index = first_non_alpha_from s start in
        let word = String.sub s start (end_index - start) in
        if String.length word = 0 then (String.sub s start 1, start)::(aux (start+1))
        else (String.sub s start (String.length word), start)::(aux end_index)
    in aux 0

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

  let concatenate s =
    let rec aux = function
      | [] -> ""
      | hd::[] -> hd
      | hd::tl -> hd ^ " " ^ (aux tl)
    in aux s

  let find_couple voc w =
    let rec aux = function
      | [] -> false
      | (k,_)::tl -> if k = w then true else aux tl
    in aux voc

  (* doesnt work, to fix *)
  let learn batch =
  let batch2 = sep_words (concatenate batch) in
  let rec learn_aux batch voc i =
    match batch with
    | [] -> voc
    | hd :: tl ->
      match find_couple voc hd with
      | true -> learn_aux tl voc i;
      | false -> learn_aux tl (voc@[(hd,i)]) (i + 1)
  in
  learn_aux batch2 [] 0


end

module TokenizerCheckType : Definitions.Tokenizer.TOKENIZER = Tokenizer
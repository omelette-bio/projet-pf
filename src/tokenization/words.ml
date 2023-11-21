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
    let rec aux s start = 
      if start >= String.length s then []
      else 
        let end_index = first_non_alpha_from s start in
        let word = String.sub s start (end_index - start) in
        if String.length word = 0 then (String.sub s start 1, start)::(aux s (start+1))
        else (String.sub s start (String.length word), start)::(aux s end_index)
    in aux s 0

  (*
  voc = [("hello", 13); (" ", 4); ("world", 7); ("!", 12)]
  s = "hello world!!"
  encode voc s = [13; 4; 7; 12; 12]
  *)
  let encode voc s =
    ignore (voc, s);
    failwith "todo"

  exception DecodingError of int

  let decode voc ids = 
    ignore (voc, ids);
    failwith "todo"

  let learn batch = 
    ignore (batch);
    failwith "todo"

end

module TokenizerCheckType : Definitions.Tokenizer.TOKENIZER = Tokenizer
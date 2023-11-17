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
    (* renvoie l'indice du premier caractère non alphabetique 
       de la chaine s en partant de l'indice start
       si ce caractère n'existe pas, renvoie la longueur de s 
       exemples:
        - `first_non_alpha_from "hello world" 3` renvoie 5
        - `first_non_alpha_from "hello world" 5` renvoie 5
        - `first_non_alpha_from "hello world" 6` renvoie 11       
       *)
    ignore (s, start);
    failwith "todo (hint)"
      

  let alpha_blocks s = 
    (* renvoie la liste des couples mot-position de la chaine s 
      exemples:
      - `alpha_blocks "hello world"` renvoie `[("hello", 0); (" ", 5); ("world", 6)]`
      - `alpha_blocks "a-b..."` renvoie 
        `[("a", 0); ("-", 1); ("b", 2); (".", 3); (".", 4); (".", 5)]`
    *)
    ignore s;
    failwith "todo (hint)"

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
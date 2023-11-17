module Tokenizer = struct

  type vocabulary = char list

  let voc_size voc = List.length voc

  exception EncodingError of string

  let encode voc s =
    ignore (voc, s);
    failwith "todo"  

  exception DecodingError of int

  let decode voc ids =
    ignore (voc, ids);
    failwith "todo"  
 
  let learn batch =
    ignore batch; 
    failwith "todo"

end

module TokenizerCheckType : Definitions.Tokenizer.TOKENIZER = Tokenizer
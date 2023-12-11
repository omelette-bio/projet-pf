open Tokenization.Words
open Tokenization.Ngrammes
open Learner
open RandomWalk

let token_of_arc src dst =
  ignore dst;
  string_of_int src

let run ~files ~window_size ~output_length = 
  let rec get_files_content = function
    | [] -> []
    | file::files -> 
      let rec get_lines ic = 
        try
          let line = input_line ic in
          line^" "^get_lines ic
        with
          | End_of_file -> ""
      in 
      get_lines (open_in file) :: get_files_content files
  in
  let files_content = get_files_content files in
  let voc = Tokenizer.learn files_content in
  let rec encode_texts = function
    | [] -> []
    | hd::tl -> Tokenizer.encode voc hd :: encode_texts tl
  in 
  let encoded = List.flatten (encode_texts files_content) in
  let encoded_ngrammes = ngrammes window_size encoded in
  let mc = learn_markov_chain ~token_of_arc:token_of_arc ~max_state_id:(List.length encoded) ~walks:encoded_ngrammes in
  let final = List.map (fun i -> int_of_string i) (random_walk ~length:output_length mc) in
  Tokenizer.decode voc final

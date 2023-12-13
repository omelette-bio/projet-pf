open MarkovChain

let () = 
  TextGenerator.run  
    ~files: ["../../../../data/jojo_eng.txt"] 
    ~window_size: 2
    ~output_length: 1005
  |> Format.printf "%s@."
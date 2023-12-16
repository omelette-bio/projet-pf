open MarkovChain

let () = 
  TextGenerator.run  
    ~files: ["../../../../data/jojo_eng.txt"] 
    ~window_size: 5
    ~output_length: 1000
  |> Format.printf "%s@."
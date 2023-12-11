open MarkovChain

let () = 
  TextGenerator.run  
    ~files: ["../../../../data/small_swann.txt";"../../../../data/yoshikagefr.txt"] 
    ~window_size: 5
    ~output_length: 500
  |> Format.printf "%s@."
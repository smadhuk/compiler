open Slp 


let rec maxargs s =
    match s with
        CompoundStm(a, b) -> max (maxargs a) (maxargs b)
    |   PrintStm(p)       -> 
          (match p with
            []   -> 0
          | x::l -> 
                (match x with
                    EseqExp(q, _) -> max (max (List.length p) (maxargs q)) (maxargs (PrintStm(l))) 
                    | _           -> max (List.length p) (maxargs (PrintStm(l))))) 
    |   AssignStm(_, a)   -> 
                (match a with
                   EseqExp(r, _) -> maxargs r
                 | _             -> 0)
;;



open Slp.SLI

(* type id = string;;
 * 
 * type binop = Plus | Minus | Times | Div;;
 * 
 * type   stm = CompoundStm of stm * stm 
 *            | AssignStm of id * exp
 *            | PrintStm of exp list
 * 
 *    and exp = IdExp of id
 *            | NumExp of int
 *            | OpExp of exp * binop * exp
 *            | EseqExp of stm * exp;;
 * 
 *
 *)
 
(* a := 5 + 3; b := (print(a,a-1), 10*a); print (b) *)  
let prog = CompoundStm(AssignStm("a", OpExp(NumExp 5, Plus, NumExp 3)), 
             CompoundStm(AssignStm("b",
               EseqExp(PrintStm[IdExp "a"; OpExp(IdExp "a", Minus,
                                                 NumExp 1)],
                  OpExp(NumExp 10, Times, IdExp "a"))),
                    PrintStm[IdExp "b"]));;


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

let test = maxargs prog;;

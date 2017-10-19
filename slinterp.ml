open Slp

type table = (id * int) list

let rec lookup (tab : table) (v : id) =
    match (tab) with
        []         -> None
    |   (x, i)::xs -> if (x = v) then i else lookup (xs) (v)

let update (tab : table) ((i, v):(id * int)) = 
    (i, v)::tab

let rec evaluateBinop(e1 : exp, b : binop, e2 : exp) (t : table) : int = 
    let
      val e1_val = interpExp(e1, t)
      val e2_val = interpExp(e2, t)
      match b with
        Plus -> e1_val + e2_val

(* interprets a statement and returns updated table *)
let rec interpStm (s : stm, t : table) : table =
    match s with
      AssignStm(i, e) -> update (t) (i, interpExp(e, t).(1))
    | PrintStm(l) -> (match l with
                       [] -> t
                     | x::xs -> interpStm(xs, interpExp(x, t).(2)))
    | CompundStm(s1, s2) -> interpStm(s2, interpStm(s1, t))
                     
(* interprets an expression and returns expression value and updated table *)
let rec interpExp (e : exp, t : table):(int * table) =
    match e with
      IdExp(i)  -> (lookup (t, i), t)
    | NumExp(i) -> (i, t)
    | OpExp(e1, b, e2) ->
            let
              val v = evaluateBinop(e1, b, e2)
              (v, update t v)
    | EseqExp(s, e) -> interpExp(interpStm(s, t))

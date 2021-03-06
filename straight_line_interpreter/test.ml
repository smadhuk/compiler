open OUnit;;
open Slp;;
open Maxargs;;

let prog = CompoundStm(AssignStm("a", OpExp(NumExp 5, Plus, NumExp 3)), 
             CompoundStm(AssignStm("b",
               EseqExp(PrintStm[IdExp "a"; OpExp(IdExp "a", Minus,
                                                 NumExp 1)],
                  OpExp(NumExp 10, Times, IdExp "a"))),
                    PrintStm[IdExp "b"]));;

let test1 = PrintStm[IdExp "a"]
let test2 = CompoundStm(PrintStm[IdExp "a"],PrintStm[IdExp "a"; IdExp "a"; IdExp "a"; IdExp "a"])

let test_sli = 
    assert_equal 2 (maxargs prog);
    assert_equal 1 (maxargs test1);
    assert_equal 4 (maxargs test2)

let _ =
    test_sli

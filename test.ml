(* imports *)
open Grammar

(*     EVALUATION TEST     *)

(** test input *)
let input: int = 5

(** factorial function written in OCaml *)
let fac n = 
  let rec fac' n m = 
    if n < 1 
    then m 
    else fac' (n-1) (n*m) 
  in fac' n 1
let result_ocaml = fac input
(** factorial function written in Mini-OCaml *)
let fac_miniocaml = 
  Let(
    "fac", 
    LetR(
      "fac'",
      "n",
      Lmda(
        "m",
        If(
          OpApp(
            LessEq,
            Var("n"),
            Con(ConInt(0))),
          Var("m"),
          FnApp(
            FnApp(
              Var("fac'"),
              OpApp(
                Sub,
                Var("n"),
                Con(ConInt(1)))),
            OpApp(
              Mul,
              Var("m"),
              Var("n"))))),
      Var("None")), 
    FnApp(
      FnApp(
        Var("fac'"), 
        Var("n")), 
      Con(
        ConInt(1))))
let result_miniocaml = Evaluator.evaluator fac_miniocaml []

(** tests if both functions result in the same output *)
let evaluation_test () = 
  if result_ocaml = result_miniocaml 
  then print_endline "Evaluation Test passed"
  else print_endline "Evaluation Test not passed, there must be some mistake!"
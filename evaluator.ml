(* imports *)
open Grammar

(** EVALUATOR - computes the value of an expression in an environment *)
let evaluator (expression: expr) (environment: 'a list) = 42


(** testing *)
let () = Test.evaluation_test ()
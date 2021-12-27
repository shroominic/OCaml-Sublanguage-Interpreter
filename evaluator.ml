(* imports *)
open Grammar

let todo_implement = IntVal 42

(** EVALUATOR - computes the value of an expression in an environment *)
let eval env e : value = 
  match e with
  | Var    (x)                  -> eval_var env x 
  | Con    (BoolCon b)          -> BoolVal b
  | Con    (IntCon i)           -> IntVal i 
  | OpApp  (o, e1, e2)          -> eval_op o (eval env e1) (eval env e2) 
  | FnApp  (e1, e2)             -> eval_fn (eval env e1) (eval env e2)
  | If     (e1, e2, e3)         -> eval_if (eval env e1) (eval env e2) (eval env e3)
  | Lmda   (x, e)
  | LmdaTy (x, _, e)            -> Closure (x, e, env)
  | Let    (x, e1, e2)          -> eval (update env x (eval env e1)) e2
  | Letrec (f, x, e1, e2) 
  | LetRTy (f, x, _, _, e1, e2) -> todo_implement
and eval_var env v = 
  let unwrap = lookup env v in 
  match unwrap with 
  | Some v -> v 
  | None   -> failwith "missing variable"
and eval_op o v1 v2 = 
  match o, v1, v2 with 
  | Add,    IntCon(x), IntCon(y) -> IntCon(x + y)
  | Sub,    IntCon(x), IntCon(y) -> IntCon(x - y)
  | Mult,   IntCon(x), IntCon(y) -> IntCon(x * y)
  | LessEq, IntCon(x), IntCon(y) -> BoolCon(x <= y)
  | _, _, _ -> failwith "operator application not supported"
and eval_fn v1 v2 = todo_implement 
and eval_if v1 v2 v3 = todo_implement



(** testing *)
let () = Test.evaluation_test ()
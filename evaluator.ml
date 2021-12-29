(* imports *)
open Grammar

(**   EVALUATOR - computes the value of an expression in an environment   *)
let rec eval env e : value = 
  match e with
  | Var    (x)                  -> eval_var env x
  | Con    (BoolCon b)          -> BoolVal b 
  | Con    (IntCon i)           -> IntVal i
  | OpApp  (o, e1, e2)          -> eval_op o (eval env e1) (eval env e2)
  | FnApp  (e1, e2)             -> eval_fun (eval env e1) (eval env e2)
  | If     (e1, e2, e3)         -> eval_if env e1 e2 e3
  | Lmda   (x, e)
  | LmdaTy (x, _, e)            -> Closure (x, e, env)
  | Let    (x, e1, e2)          -> eval (update env x (eval env e1)) e2
  | LetR   (f, x, e1, e2) 
  | LetRTy (f, x, _, _, e1, e2) -> eval (update env f (Rclosure (f, x, e1, env))) e2 
(** variable evaluation *)
and eval_var env v = 
  let unwrap = lookup env v in 
  match unwrap with 
  | Some v -> v 
  | None   -> failwith "eval_var: unbound variable"
(** operator evaluation *)
and eval_op o v1 v2 = 
  match o, v1, v2 with 
  | Add,    IntVal(x), IntVal(y) -> IntVal(x + y)
  | Sub,    IntVal(x), IntVal(y) -> IntVal(x - y)
  | Mul,    IntVal(x), IntVal(y) -> IntVal(x * y)
  | LessEq, IntVal(x), IntVal(y) -> BoolVal(x <= y)
  | _, _, _ -> failwith "eval_op_app: ill-typed arguments"
(** function evaluation *)
and eval_fun v1 v2 = 
  match v1 with
  | Closure (x, e, env) -> eval (update env x v2) e
  | Rclosure(f, x, e, env) -> eval (update (update env f v1) x v2) e 
  | _ -> failwith "eval_fun_app: function expected"
(** if-then-else evaluation *)
and eval_if env e1 e2 e3 = 
  let v1 = eval env e1 in
  match v1 with
  | BoolVal(true) -> eval env e2
  | BoolVal(false) -> eval env e3
  | _ -> failwith "eval_if: boolean expected"


(**   TESTING   *)

(** test input *)
let input: int = 17
let () = print_endline ("Factorial(" ^ (string_of_int input) ^ ")")

(** factorial function written in OCaml *)
let result_ocaml = (let fac = 
                      (fun n -> 
                         let rec fac' n = 
                           (fun m -> 
                              if n < 1 
                              then m 
                              else fac' (n-1) (n*m))

                         in fac' n 1) in fac) input

let () = print_endline ("OCaml       : " ^ (string_of_int result_ocaml))

(** factorial function written in Mini-OCaml *)
let fac_miniocaml = 
  FnApp(
    Let(
      "fac",
      LmdaTy(
        "n",
        Int,
        LetRTy(
          "fac'",
          "n",
          Int,
          Arrow(Int, Int),
          LmdaTy(
            "m",
            Int,
            If(
              OpApp(
                LessEq,
                Var("n"),
                Con(IntCon(1))),
              Var("m"),
              FnApp(
                FnApp(
                  Var("fac'"),
                  OpApp(
                    Sub,
                    Var("n"),
                    Con(IntCon(1)))),
                OpApp(
                  Mul,
                  Var("n"),
                  Var("m"))))),
          FnApp(
            FnApp(
              Var("fac'"), 
              Var("n")), 
            Con(IntCon(1))))), 
      Var("fac")),
    Con(IntCon(input)))

let result_miniocaml = 
  let evaluation = eval empty fac_miniocaml in 
  match evaluation with
  | IntVal (x) -> x
  | _ -> failwith "unable to unwrap type"
let () = print_endline ("Mini-OCaml  : " ^ (string_of_int result_miniocaml))

(** tests if both functions result in the same output *)
let () = 
  if result_ocaml = result_miniocaml 
  then print_endline "Evaluation test passed"
  else print_endline "Evaluation test not passed, there must be some mistake!"

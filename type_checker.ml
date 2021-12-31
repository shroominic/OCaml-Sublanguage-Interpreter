(* imports *)
open Grammar

(** TYPE CHECKER - computes the type of an expression in an environment *)
let rec check env expr : btype = 
  match expr with
  | Var    (x)                    -> check_var env x
  | Con    (BoolCon b)            -> Bool
  | Con    (IntCon i)             -> Int
  | OpApp  (o, e1, e2)            -> check_op o (check env e1) (check env e2)
  | FnApp  (e1, e2)               -> check_fun (check env e1) (check env e2)
  | If     (e1, e2, e3)           -> check_if (check env e1) (check env e2) (check env e3)
  | Lmda   (_, _)                 -> failwith "check_lambda: missing type"
  | LmdaTy (x, t, e)              -> Arrow (t, check (update env x t) e)
  | Let    (x, e1, e2)            -> check (update env x (check env e1)) e2
  | LetR   (f, x, e1, e2)         -> failwith "check_let_rec: missing type"
  | LetRTy (f, x, t1, t2, e1, e2) -> check_let_rec_ty f x t1 t2 e1 e2 env
and check_var env v = 
  let unwrap = lookup env v in 
  match unwrap with 
  | Some v -> v
  | None -> failwith ("check_var: unbound variable")
and check_op op t1 t2 = 
  match op, t1, t2 with
  | Add, Int, Int     -> Int
  | Sub, Int, Int     -> Int
  | Mul, Int, Int     -> Int
  | LessEq, _, _  -> Bool
  | _, _, _ -> failwith "check_operator_app: ill-typed arguments"
and check_fun t1 t2 = 
  match t1 with
  | Arrow (t2', t) -> if t2 = t2' then t
    else failwith "check_function_app: wrong argument type"
  | _ -> failwith "check_function_app: function expected"
and check_if t1 t2 t3 =
  match t1 with
  | Bool -> if t2 = t3 then t2
    else failwith "check_if: both branches expect to be the same type"
  | _ -> failwith "check_if: bool expected"
and check_let_rec_ty f x t1 t2 e1 e2 env = 
  let env' = update env f (Arrow (t1, t2)) in
  if check (update env' x t1) e1 = t2 
  then check env' e2
  else failwith "check_let_rec: declared type not matched"


(**   TESTING   *)
let test_type_checker () = 

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
      Con(IntCon(5))) in

  let type_miniocaml = check empty fac_miniocaml in

  (** tests if both functions result in the same type *)
  if type_miniocaml = Int
  then print_endline "TypeChecking test passed"
  else print_endline "TypeChecking test not passed, there must be some mistake!"
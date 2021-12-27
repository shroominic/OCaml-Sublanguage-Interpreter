(* imports *)
open Grammar

(** TYPE CHECKER - computes the type of an expression in an environment *)
let rec check env expr : btype = 
  match expr with
  | Var    (x)                    -> check_var env x
  | Con    (BoolCon b)            -> Bool
  | Con    (IntCon i)             -> Int
  | OpApp  (o, e1, e2)            -> check_op o (check env e1) (check env e2)
  | FnApp  (e1, e2)               -> check_fn (check env e1) (check env e2)
  | If     (e1, e2, e3)           -> check_if (check env e1) (check env e2) (check env e3)
  | Lmda   (_, _)                 -> failwith "lambda: missing type"
  | LmdaTy (x, t, e)              -> Arrow (t, check (update env x t) expr)
  | Let    (x, e1, e2)            -> check (update env x (check env e1)) e2
  | LetR   (f, x, e1, e2)         -> failwith "let rec: missing type"
  | LetRTy (f, x, t1, t2, e1, e2) -> Int
and check_var env v = 
  let unwrap = lookup env v in 
  match unwrap with 
  | Some v -> v 
  | None -> failwith "missing variable"
and check_op op t1 t2 = 
  if t1 = t2 then
    match op with
    | Add -> Int
    | Sub -> Int
    | Mul -> Int
    | LessEq -> Bool
  else failwith "both types must be the same"
and check_fn t1 t2 =
  match t1 with
  | Arrow (t1', t2') -> if t1' = t2 then t2' else failwith "function cannot be applied"
  | _ -> failwith "function cannot be applied"
and check_if t1 t2 t3 =
  match t1 with
  | Bool -> 
    if t2 = t3 
    then t2 
    else failwith "both condition branches must have the same type"
  | _ -> failwith "condition must be type bool"


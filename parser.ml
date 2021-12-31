(* imports *)
open Grammar

(** verify token t as the next element of list l *)
let verify t l =
  match l with
  | [] -> failwith "verify: no token"
  | t' :: l -> if t' = t then l else failwith "verify: wrong token"


let rec exp l : expr * token list =
  match l with
  | IF :: l ->
    let e1, l = exp l in
    let e2, l = exp (verify THEN l) in
    let e3, l = exp (verify ELSE l) in
    (If (e1, e2, e3), l)
  | LAM :: VAR x :: ARR :: l ->
    let e, l = exp l in
    (Lmda (x, e), l)
  | LET :: VAR x :: EQ :: l ->
    let e1, l = exp l in
    let e2, l = exp (verify IN l) in
    (Let (x, e1, e2), l)
  | LET :: REC :: VAR f :: VAR x :: EQ :: l ->
    let e1, l = exp l in
    let e2, l = exp (verify IN l) in
    (LetR (f, x, e1, e2), l)
  | l -> cexp l
(** comparisons *)
and cexp l =
  let e, l = sexp l in
  cexp' e l
and cexp' e1 l =
  match l with
  | LEQ :: l ->
    let e2, l = sexp l in
    (OpApp (LessEq, e1, e2), l)
  | l -> (e1, l)
(** additive operators *)
and sexp l =
  let e, l = mexp l in
  sexp' e l
and sexp' e1 l =
  match l with
  | ADD :: l ->
    let e2, l = mexp l in
    sexp' (OpApp (Add, e1, e2)) l
  | SUB :: l ->
    let e2, l = mexp l in
    sexp' (OpApp (Sub, e1, e2)) l
  | l -> (e1, l)
(** multiplicative operators *)
and mexp l =
  let e, l = aexp l in
  mexp' e l
and mexp' e1 l =
  match l with
  | MUL :: l ->
    let e2, l = aexp l in
    aexp' (OpApp (Mul, e1, e2)) l
  | l -> (e1, l)
(** function application *)
and aexp l =
  let e, l = pexp l in
  aexp' e l
and aexp' e1 l =
  match l with
  | CON _ :: _ | VAR _ :: _ | LP :: _ ->
    let e2, l = pexp l in
    aexp' (FnApp (e1, e2)) l
  | l -> (e1, l)
(** bottom level *)
and pexp l =
  match l with
  | CON (BCON b) :: l -> (Con (BoolCon b), l)
  | CON (ICON n) :: l -> (Con (IntCon n), l)
  | VAR x :: l -> (Var x, l)
  | LP :: l ->
    let e, l = exp l in
    (e, verify RP l)
  | _ -> failwith "pexp"

(** PARSER - translates list of tokens into abstract expressions *) 
let parser input : expr = fst (exp input)
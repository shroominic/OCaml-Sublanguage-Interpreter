(*    ABSTRACT EXPRESSIONS    *)
(** variable *) 
type var = string 

(** constant *) 
type con = 
    BoolCon of bool
  | IntCon  of int

(** operator *)
type opr  = 
    Add 
  | Sub
  | Mul 
  | LessEq

(** basic type *)
type btype = 
    Bool
  | Int 
  | Arrow of btype * btype

(** expression *)
type expr = 
    Var     of var
  | Con     of con 
  | OpApp   of opr * expr * expr  
  | FnApp   of expr * expr        
  | If      of expr * expr * expr 
  | Lmda    of var * expr        
  | LmdaTy  of var * btype * expr 
  | Let     of var * expr * expr  
  | LetR    of var * var * expr * expr 
  | LetRTy  of var * var * btype * btype * expr * expr

(*    ENVIRONMENTS    *)
(** environment constructor type *)
type ('a, 'b) env = ('a * 'b) list

(** empty environment definition *)
let empty: ('a, 'b) env = []

(** updates an environment with a value pair (a, b) *)
let update (env: ('a, 'b) env) a b : ('a, 'b) env = (a, b) :: env

(** lookup recursivly for a key (a) and optional returns the corresponding value b *)
let rec lookup (env: ('a, 'b) env) a = 
  match env with
  | (a', b)::env -> if a = a' then Some b else lookup env a
  | [] -> None

(*    VALUES    *)
(** value constructor type *)
type value = 
    BoolVal of bool
  | IntVal of int
  | Closure of var * expr * (var, value) env
  | Rclosure of var * var * expr * (var, value) env

(*    TOKENS    *)
(** token constants *)
type const = 
    BCON of bool 
  | ICON of int
(** token constructor type *)
type token = 
    LP 
  | RP 
  | EQ 
  | COL 
  | ARR 
  | ADD 
  | SUB 
  | MUL 
  | LEQ
  | IF 
  | THEN 
  | ELSE 
  | LAM 
  | LET
  | IN 
  | REC
  | CON of const 
  | VAR of string 
  | BOOL 
  | INT
(* ABSTRACT EXPRESSION TYPES *)
(** variable *) 
type var = string 

(** constant *) 
type con = 
    ConBool of bool
  | ConInt  of int

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

(* ENVIRONMENTS *)
(** environment type *)
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

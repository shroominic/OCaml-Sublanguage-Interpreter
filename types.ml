(* TOKEN TYPES *)
type token = TOKEN


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

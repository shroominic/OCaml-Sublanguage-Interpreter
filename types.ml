(* TOKEN TYPES *)
type token = TOKEN


(* ABSTRACT EXPRESSION TYPES *)
(** variable *) 
type var = string 

(** constant *) 
type con = 
    ConBool of bool |
    ConInt  of int

(** operator *)
type opr  = 
    Add | 
    Sub | 
    Mul | 
    LessEq

(** expression *)
type expr = 
    Var     of var |
    Con     of con | 
    OpApp   of opr * expr * expr  | 
    FnApp   of expr * expr        | 
    If      of expr * expr * expr | 
    Lambda  of var * expr         |
    Let     of var * expr * expr  |
    LetRec  of var * var * expr * expr 
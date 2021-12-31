(* 
This is my own (mini) OCaml Interpreter.
LEXER -> PARSER -> TYPE CHECKER -> EVALUATOR
*)

(* imports *)
open Grammar
open Lexer
open Parser
open Type_checker
open Evaluator

(** input string - factorial(17) *)
let fac_string =
  " let rec fac a = fun n ->     \n\
  \     if n <= 1                \n\
  \     then a                   \n\
  \     else fac (n*a) (n-1) in  \n\
  \     fac 1 17 "

(** parsing *)
let lexed_string = lex fac_string
let parsed_string = parser lexed_string

(** typecheck string: string -> type *)
let check_string str : btype = check empty parsed_string

(** evaluate string: string -> value *)
let eval_string str : value = eval empty parsed_string

(** value -> int *)
let unwrap_int_value v : int = 
  match v with
  | IntVal (x) -> x
  | _ -> failwith "unable to unwrap type"

(** result of factorial(17) -> int *)
let result = unwrap_int_value (eval_string fac_string)

(** print the final result *)
let () = print_endline ("Factorial(17): " ^ (string_of_int result))

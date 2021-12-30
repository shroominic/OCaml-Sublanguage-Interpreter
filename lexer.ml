(* imports *)
open Grammar


(** ascii code representation of a char *)
let code = Char.code

(** converts char_digit -> int *)
let num c = code c - code '0'

(** checks if char c is a digit *)
let digit c = code '0' <= code c && code c <= code '9'

(** checks if char c is a lower_case letter *)
let lc_letter c = code 'a' <= code c && code c <= code 'z'

(** checks if char c is a UPPER_CASE letter *)
let uc_letter c = code 'A' <= code c && code c <= code 'Z'

(** checks if char c is a whitespace/tab/new_line *)
let whitespace c = 
  match c with
  | ' ' 
  | '\n' 
  | '\t' -> true
  | _    -> false

(** LEXER - translates strings into list of tokens *)
let lex s : token list =
  (* string -> char at index i *)
  let get i = 
    String.get s i in
  (* string s -> string_slice at index i, length n *)
  let getstr i n = 
    String.sub s (i-n) n in
  (* checks if index i is in bounds of string s *)
  let exhausted i = 
    i >= String.length s in
  (* verify if char c is at index i and can get accessed *)
  let verify i c = 
    not (exhausted i) && get i = c in
  (* main lexing function *)
  let rec lex i l =
    if exhausted i then List.rev l
    else match get i with
      | '+' -> lex (i+1) (ADD::l)
      | '*' -> lex (i+1) (MUL::l)
      | '=' -> lex (i+1) (EQ::l)
      | '(' -> lex (i+1) (LP::l)
      | ')' -> lex (i+1) (RP::l)
      | '<' -> if verify (i+1) '='
               then lex (i+2) (LEQ::l)
               else failwith "lexer: '=' expected"
      | '-' -> if verify (i+1)
               then lex (i+2) (ARR::l)
               else lex (i+1) (SUB::l)
      | c when whitespace c -> lex (i+1) l
      | c when digit c      -> lex_num (i+1) (num c) l
      | c when lc_letter c  -> lex_id (i+1) 1 l
      | c -> failwith "lexer: illegal character"
  and lex_num i n l = 
    if exhausted i 
    then lex_num' i n l
    else let c = get i in
      if digit c 
      then lex_num (i+1) (10*n + num c) l
      else lex_num' i n l
  and lex_num' i n l = lex i (CON (ICON n)::l)
  and lex_id i n l = 
    if exhausted i 
    then lex_id' i n l
    else match get i with
      | '\'' 
      | '_' -> lex_id (i+1) (n+1) l
      | c   -> 
        if lc_letter c || uc_letter c || digit c
        then lex_id (i+1) (n+1) l
        else lex_id' i n l
  and lex_id' i n l = match getstr i n with
    | "if"    -> lex i (IF::l)
    | "then"  -> lex i (THEN::l)
    | "else"  -> lex i (ELSE::l)
    | "fun"   -> lex i (LAM::l)
    | "let"   -> lex i (LET::l)
    | "in"    -> lex i (IN::l)
    | "rec"   -> lex i (REC::l)
    | "false" -> lex i (CON (BCON false)::l)
    | "true"  -> lex i (CON (BCON true)::l)
    | s       -> lex i (VAR s::l)
  in lex 0 []


  (* TESTING *)
  (* todo_implement *)
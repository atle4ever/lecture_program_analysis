exception Error of string

(* AST of K language *)
type id = string
type label = int
type cmd = label * stmt
and stmt = SKIP
	   | ASSIGN of id * exp
	   | ASSIGNSTAR of id * exp
	   | SEQ of cmd * cmd
	   | IF of exp * cmd * cmd
	   | WHILE of exp * cmd
       | END
and exp = NUM of int 
	  | TRUE
	  | FALSE
	  | ADD of exp * exp
	  | MINUS of exp
	  | VAR of id
	  | STAR of id
	  | AMPER of id
	  | READ
	  | LESS of exp * exp
type program = cmd

let rec string_of_exp e = match e with
    NUM i -> string_of_int i
  | ADD (e1, e2) -> "("^(string_of_exp e1)^" + "^(string_of_exp e2)^")"
  | MINUS (e1) -> "-("^(string_of_exp e1)^")"
  | VAR x -> x
  | STAR x -> "* "^x
  | AMPER x -> "& "^x
  | READ -> "read"
  | TRUE -> "true"
  | FALSE -> "false"
  |	LESS (e1, e2) -> "("^(string_of_exp e1)^" + "^(string_of_exp e2)^")"

and string_of_stmt s = match s with
    SKIP -> "skip"
  | ASSIGN(x, e) -> x^" := "^(string_of_exp e)
  | ASSIGNSTAR(x, e) -> "*"^x^" := "^(string_of_exp e)
  | SEQ(c1, c2) -> (string_of_cmd c1)^";\n"^(string_of_cmd c2)
  | IF(e, c1, c2) -> "if ("^(string_of_exp e)^")"^(string_of_cmd c1)^"\n"^(string_of_cmd c2)
  | WHILE(e, c) -> "while "^(string_of_exp e)^"\n"^(string_of_cmd c)
  | END -> "end"
and string_of_cmd (l, stmt) =
  (string_of_int l)^": "^(string_of_stmt stmt)

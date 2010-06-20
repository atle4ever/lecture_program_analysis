{
  open Parser

  let keyword_tbl = Hashtbl.create 31
  let _ = List.iter (fun (keyword, tok) -> Hashtbl.add keyword_tbl keyword tok)
    [("if", IF);
     ("while", WHILE);
	 ("true", TRUE);
	 ("false", FALSE);
     ("read", READ);
     ("skip", SKIP);
    ]
}

  let id = ['a'-'z' 'A'-'Z'](['a'-'z' 'A'-'Z' '\'' '0'-'9' '_'])*

  rule token = parse
      [' ' '\t' '\n' '\r'] {token lexbuf }
    | ['0'-'9']+ as lxm { INT (int_of_string lxm) }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | ":=" { ASSIGN }
    | "*" { STAR }
    | ";" { SEMICOLON }
    | "+" { PLUS }
	| "<" { LESS }
    | "-" { MINUS }
    | "&" { AMPER }
    | id { let id = Lexing.lexeme lexbuf
           in try Hashtbl.find keyword_tbl id
             with _ -> ID id
         }
    | eof { EOF }



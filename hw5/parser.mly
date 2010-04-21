%{
  let label = ref (-1)
%}
%token <int> INT
%token <string> ID
%token SKIP ASSIGN STAR SEMICOLON IF WHILE PLUS MINUS AMPER READ EOF
%token LPAREN RPAREN
%start main
%type <K.cmd> main
%type <K.cmd> cmd
%type <K.exp> expr
%left PLUS SEMICOLON
%nonassoc IF WHILE
%nonassoc MINUS
%%

main:
  cmd EOF { $1 }

cmd:
SKIP {incr label; (!label, K.SKIP) }
| LPAREN cmd RPAREN { $2 }
| ID ASSIGN expr {incr label; (!label, K.ASSIGN($1, $3)) }
| STAR ID ASSIGN expr { incr label; (!label, K.ASSIGNSTAR($2, $4)) }
| cmd SEMICOLON cmd { incr label; (!label, K.SEQ($1, $3)) }
| IF expr cmd cmd { incr label; (!label, K.IF($2, $3, $4)) }
| WHILE expr cmd { incr label; (!label, K.WHILE($2, $3)) }

expr:
INT { K.NUM $1 }
| LPAREN expr RPAREN { $2 }
| expr PLUS expr { K.ADD($1, $3) }
| MINUS expr { K.MINUS($2) }
| ID { K.VAR $1 }
| STAR ID { K.STAR $2 }
| AMPER ID { K.AMPER $2 }
| READ { K.READ }

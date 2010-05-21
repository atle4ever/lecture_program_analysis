open Analyzer

let rec get_max_label (l, stmt) =
  match stmt with
      K.SEQ (c1, c2)
    | K.IF (_, c1, c2) ->
        max l (max (get_max_label c1)
                 (get_max_label c2))
    | K.WHILE (_, c) ->
        max l (get_max_label c)
    | _ -> l

let print_next pgm l =
  print_endline ("next of " ^ string_of_int l ^ ": ");
  print_endline (K.string_of_cmd (Analyzer.n pgm l))

let print_all_next pgm m =
  let rec _print_all_next i =
    if i > m then ()
    else (print_next pgm i;_print_all_next (i + 1))
  in
    _print_all_next 0

let main = 
  let lexbuf = Lexing.from_channel stdin in
  let c = Parser.main Lexer.token lexbuf in
	print_endline "=== INPUT PROGRAM ===";
	print_string (K.string_of_cmd c); 
	print_newline ();
	print_endline "=== NEXT COMMANDS ===";
	print_all_next c (get_max_label c);

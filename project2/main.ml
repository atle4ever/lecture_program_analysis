open Analyzer

let main = 
  let lexbuf = Lexing.from_channel stdin in
  let c = Parser.main Lexer.token lexbuf in
  let sol = Analyzer.analyze c in
  let vars = Analyzer.get_all_davinci_vars sol in
	print_endline "=== INPUT PROGRAM ===";
	print_string (K.string_of_cmd c); 
	print_newline ();
	print_endline "=== DAVINCI VARIABLES ===";
	List.iter (fun v -> print_endline v) vars

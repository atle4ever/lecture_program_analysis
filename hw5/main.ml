let rec print_trace trace =
	(match trace 
	with h::t -> print_string ("  {\n"^(Memory.string_of_memory h)) ; print_string "  }\n"; (print_trace t)
	| [] -> print_string "")
in
let rec print_traces traces = 
	(match traces
	with h::t -> print_string "[\n"; (print_trace h); print_string "];\n"; (print_traces t)
	| [] -> print_newline ()
	)
in
let print_collect set = 
	K.MemorySet.iter (fun e -> print_string ("  {\n"^(Memory.string_of_memory e)^"  }\n")) set
in
let pnt = ref 0 in
let src = ref "" in
let _ = 
	Arg.parse
		[("-p", (Arg.Int (fun x -> pnt := x)), "point-collecting eval")]
		(fun x -> src := x)
		("Usage : "^(Filename.basename Sys.argv.(0))^" [-option] [filename] ")
in
let lexbuf = Lexing.from_channel (if !src = "" then stdin else open_in !src) in
let c = Parser.main Lexer.token lexbuf in

print_endline "=== INPUT PROGRAM ===";
print_string (K.string_of_cmd c); 
print_newline ();
print_endline "=== TRACING EVAL ===";
print_string "[\n";
print_traces (K.tracingEval c Memory.empty);
print_string "]\n\n";
print_endline "=== COLLECTING EVAL ===";
print_string "[\n";
print_collect (K.collectingEval c Memory.empty);
print_string "]\n\n";
print_endline ("=== POINT-COLLECTING EVAL @ label "^(string_of_int !pnt)^" ===");
print_string "[\n";
print_collect ((K.pointCollectingEval c Memory.empty) !pnt);
print_string "]\n\n";

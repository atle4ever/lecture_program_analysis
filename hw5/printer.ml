(* value
  string_of_value is in value.ml *)
let print_value v =
  print_string (Value.string_of_value v); print_newline ()

(* memory
  string_of_memory is in memory.ml *)
let print_memory m =
  print_string (Memory.string_of_memory m); print_newline ()

(* MemorySet *)
let string_of_ms ms =
  K.MemorySet.fold (fun m str-> str^"  {\n"^(Memory.string_of_memory m)^"  }\n") ms ""

let print_ms ms =
  print_string (string_of_ms ms); print_newline ()

(* ValueSet *)
let string_of_vs vs =
  (K.VS.fold (fun v str-> str^(Value.string_of_value v)^" ") vs "{ ")
    ^ "}"

let print_vs vs =
  print_string (string_of_vs vs); print_newline ()

(* LabelMemroySetMap *)
let string_of_lmm lmm =
    (if
        K.LMM.is_empty lmm then "    Empty\n"
    else
        K.LMM.fold (fun l ms str-> str^"    "^(string_of_int l)^" : "^(string_of_ms ms)^"\n") lmm "")

let print_lmm lmm =
  print_string (string_of_lmm lmm); print_newline()

(* exp *)
let print_exp e =
  print_string (K.string_of_exp e); print_newline()


(*
load_printer "value.cmo"
load_printer "memory.cmo"
load_printer "parser.cmo"
load_printer "lexer.cmo"
load_printer "k.cmo"
load_printer "my_printer.cmo"

install_printer My_printer.printer_value
install_printer My_printer.printer_memory
install_printer My_printer.printer_memory2
install_printer My_printer.printer_ms
install_printer My_printer.printer_ll
*)

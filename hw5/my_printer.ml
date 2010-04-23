let printer_value v =
  print_string (Value.string_of_value v); print_newline ()

let printer_memory m =
  print_string (Memory.string_of_memory m); print_newline ()

let printer_memory2 f m =
  Format.fprintf f "%s @\n" (Memory.string_of_memory m)

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
*)

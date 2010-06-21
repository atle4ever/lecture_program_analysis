open Analyzer
open Domain
open Functors

let rec get_max_label (l, stmt) =
  match stmt with
      K.SEQ (c1, c2)
    | K.IF (_, c1, c2) ->
        max l (max (get_max_label c1)
                 (get_max_label c2))
    | K.WHILE (_, c) ->
        max l (get_max_label c)
    | _ -> l

let print_next n pgm l =
  print_endline ("next of " ^ string_of_int l ^ ": ");
  print_endline (K.string_of_cmd (n pgm l))

let print_all_next n pgm m =
  let rec _print_all_next i =
    if i > m then ()
    else (print_next n pgm i;_print_all_next (i + 1))
  in
    _print_all_next 0

let print_all f g to_str sol l vars =
  let print_one_var s x =
    print_endline (x ^ ": " ^ to_str (g x s))
  in
  let print_state i =
    List.iter (print_one_var (f i sol)) vars
  in
  let rec print_all_per_label i =
    if i <= l
    then
      begin
        print_endline ("=== LABEL " ^ string_of_int i ^ " ===");
        print_state i;
        print_all_per_label (i + 1)
      end
    else ()
  in
    print_all_per_label 0;
    print_endline ("=== LABEL -1 ===");
    print_state (-1)

let rec init_list n =
    if n = 0
    then [n]
    else n::(init_list (n - 1))

let rec distinct l =
  match l with
      [] -> []
    | (a::l') -> a::(distinct (drop a l'))
and drop a l =
  match l with
      [] -> []
    | (a'::l') -> if a' = a then drop a l' else l

let main =
  let lexbuf = Lexing.from_channel (open_in "test.k") in
  let c = Parser.main Lexer.token lexbuf in
  let max_label = get_max_label c in
  let vars = K.vars_of_cmd c in
  let vars' = distinct (List.sort compare vars) in
  let labels = (-1)::init_list max_label in
  let module Loc = PrimitiveSet (
      struct
          type t = string
          exception TooMany
          let compare = compare
          let all = fun () -> vars'
      end
          )
  in
  let module Label = PrimitiveSet (
      struct
          type t = int
          exception TooMany
          let compare = compare
          let all = fun () -> labels
      end
          )
          in
  let module Analyzer = Analyzer (Loc) (Label) in
  let _ = print_endline "=== INPUT PROGRAM ===" in
  let _ = print_string (K.string_of_cmd c) in
  let _ = print_newline () in
  let sol = Analyzer.analyze c in
  let vars = Analyzer.get_all_davinci_vars sol in
    print_endline "=== INPUT PROGRAM ===";
    print_string (K.string_of_cmd c);
    print_newline ();
    print_endline "=== DAVINCI VARIABLES ===";
    List.iter (fun v -> print_endline v) vars

open Analyzer
open Domain

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

      (*
module LS = Set.Make(struct type t = string let compare = compare end)

let rec get_ls_e e =
  match e with
      K.NUM _ | K.TRUE | K.FALSE | K.READ -> LS.empty
    | K.ADD (e1, e2) -> LS.union (get_ls_e e1) (get_ls_e e2)
    | K.MINUS e1 -> get_ls_e e1
    | K.VAR id | K.STAR id | K.AMPER id -> LS.singleton id

let rec get_ls_be e =
  match e with
      K.LESS (e1, e2) -> LS.union (get_ls_e e1) (get_ls_e e2)

let rec get_ls (l, stmt) =
  match stmt with
      K.SKIP -> LS.empty
    | K.ASSIGN (id, e) | K.ASSIGNSTAR(id, e) -> LS.union (LS.singleton id) (get_ls_e e)
    | K.SEQ (c1, c2) -> LS.union (get_ls c1) (get_ls c2)
    | K.IF (be, c1, c2) -> LS.union (get_ls_be be) (LS.union (get_ls c1) (get_ls c2))
    | K.WHILE (be, c1) -> LS.union (get_ls_be be ) (get_ls c1)
    | K.END -> LS.empty

let print_result ls m =
  let _print_result id m =
    try
      (
        let v = Domain.M.lookup id m in
          match v with
              Val.Z (p, i) ->
                print_string (id ^ "-> (" ^ (Parity.string_of p) ^ "," ^ (Interval.string_of i) ^ ")");
                print_newline()
            | Val.TOP ->
                print_string (id ^ "-> (" ^ (Parity.string_of Parity.top) ^ "," ^ (Interval.string_of Interval.top) ^ ")");
                print_newline()
            | _ -> print_string (id ^ " is not number"); print_newline()
      )
    with Not_found -> ()
  in
    LS.iter (fun e -> _print_result e m) ls


let print_all_result pgm sol m =
  let rec _print_all_result i =
    if i > m then ()
    else
      let m = Analyzer.get_state i sol in
        print_string (M.string_of m); print_newline(); _print_all_result (i+1)
  in
    _print_all_result 0
      *)

let main =
  let lexbuf = Lexing.from_channel stdin in
  let c = Parser.main Lexer.token lexbuf in
    print_endline "=== INPUT PROGRAM ===";
    print_string (K.string_of_cmd c);
    print_newline ();
    print_endline "=== NEXT COMMANDS ===";
    print_all_next c (get_max_label c);
    Analyzer.analyze c



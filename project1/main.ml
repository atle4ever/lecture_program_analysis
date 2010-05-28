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


module IDS = Set.Make(struct type t = string let compare = compare end)

let rec get_ids_e e =
  match e with
      K.NUM _ | K.TRUE | K.FALSE | K.READ -> IDS.empty
    | K.ADD (e1, e2) -> IDS.union (get_ids_e e1) (get_ids_e e2)
    | K.MINUS e1 -> get_ids_e e1
    | K.VAR id | K.STAR id | K.AMPER id -> IDS.singleton id
    | K.LESS (e1, e2) -> IDS.union (get_ids_e e1) (get_ids_e e2)

let rec get_ids (l, stmt) =
  match stmt with
      K.SKIP -> IDS.empty
    | K.ASSIGN (id, e) | K.ASSIGNSTAR(id, e) -> IDS.union (IDS.singleton id) (get_ids_e e)
    | K.SEQ (c1, c2) -> IDS.union (get_ids c1) (get_ids c2)
    | K.IF (e, c1, c2) -> IDS.union (get_ids_e e) (IDS.union (get_ids c1) (get_ids c2))
    | K.WHILE (e, c1) -> IDS.union (get_ids_e e ) (get_ids c1)
    | K.END -> IDS.empty

let rec seq a b =
  if a > b then []
  else a :: (seq (a+1) b)

let main =
  let lexbuf = Lexing.from_channel stdin in
  let c = Parser.main Lexer.token lexbuf in
    print_endline "=== INPUT PROGRAM ===";
    print_string (K.string_of_cmd c);
    print_newline ();
    print_endline "=== NEXT COMMANDS ===";
    print_all_next c (get_max_label c);
    let sol = Analyzer.analyze c in
    let ls = seq (-1) (get_max_label c) in
    let ids = get_ids c in
      List.iter (fun l ->
                   let s = Analyzer.get_state l sol in
                     print_string "Label "; print_int l; print_string ":\n";
                     IDS.iter (fun id ->
                                 let p = Analyzer.get_parity_of id s in
                                 let i = Analyzer.get_interval_of id s in
                                   print_string "\t"; print_string id; print_string " - ";
                                   print_string (Parity.string_of p); print_string ", ";
                                   print_string (Interval.string_of i); print_newline()
                              ) ids
                ) ls




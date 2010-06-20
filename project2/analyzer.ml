(* Analyzer code *)    
open K

module type ANALYZER =
sig
  type state
  type solution

  val n : program -> label -> cmd
  val analyze : program -> solution
  val get_all_davinci_vars : solution -> id list
end

module Analyzer : ANALYZER =
struct
  type state
  type solution = label -> state

(*
  Returns next command of the given label.
  All given labels are assumed to be valid.
  If there is no command left, END is returned.
*)
  let n : program -> label -> cmd
  = fun pgm target ->
	let rec find : cmd -> cmd list -> cmd list
	= fun ((l, c) as cmd) stack ->
        if target = l then stack
        else
          match c with
              SKIP | ASSIGN _ | ASSIGNSTAR _ | END -> []
            | SEQ (c1, c2) -> find c1 (cmd::stack) @ find c2 stack
	        | IF (_, c1, c2) -> find c1 stack @ find c2 stack
	        | WHILE (_, c1) -> find c1 (cmd::stack)
    in
    let rec get_next : cmd list -> cmd
    = fun cs ->
        match cs with
            [] -> (-1, END)
          | ((_, SEQ (c1, c2)))::cs -> c2
          | ((_, WHILE (_, _)) as c)::cs -> c
          | _::cs -> get_next cs
  in
    get_next (find pgm [])

  let analyze pgm = raise (Error "Not implemented")
  let get_all_davinci_vars sol = raise (Error "Not implemented")
end

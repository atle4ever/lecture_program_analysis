(*
 * Department  : EE
 * Student No. : 2009-20769
 * Name        : Kim, Seongjun
 * Exercise 6
 *)

type expr = NUM of int
            | PLUS of expr * expr
            | MINUS of expr * expr
            | MULT of expr * expr
            | DIVIDE of expr * expr
            | MAX of expr list

let rec eval expr =
  match expr with
      NUM i -> i
    | PLUS (e1, e2) -> (eval e1) + (eval e2)
    | MINUS (e1, e2) -> (eval e1) - (eval e2)
    | MULT (e1, e2) -> (eval e1) * (eval e2)
    | DIVIDE (e1, e2) -> (eval e1) / (eval e2)
    | MAX [] -> 0
    | MAX [e] -> eval e
    | MAX (e::es) ->
        let v1 = eval e in
        let v2 = eval (MAX es) in
          if v1 > v2 then
            v1
          else
            v2

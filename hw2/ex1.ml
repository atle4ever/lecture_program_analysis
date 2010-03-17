(*
 * Department  : EE
 * Student No. : 2009-20769
 * Name        : Kim, Seongjun
 * Exercise 1
 *)

(* CHECK
   1. DIV(INT 10, INT 4) = 2.5? 2?
   2. exception? FreeVariable, InvalidSigma, DivideByZero
   3. SIGMA(REAL 1.0, REAL 10.9, X) = 55.0 ?
   4. INTEGRAL(REAL 1.0, REAL 1.25, X) = ? [1.0, 1.1), [1.1, 1.2), [1.2, 1.25) ?
   5. INTEGRAL(REAL 2.0, REAL 1.0, X) = -1 * INTEGRAL(REAL 1.0, REAL 2.0, X)
*)

type exp = X
       | INT of int
       | REAL of float
       | ADD of exp * exp
       | SUB of exp * exp
       | MUL of exp * exp
       | DIV of exp * exp
       | SIGMA of exp * exp * exp
       | INTEGRAL of exp * exp * exp

type var = UNBINDED
       | VAR of float

exception FreeVariable
exception InvalidSigma
exception DivideByZero

let mathemadiga exp =
  let rec var_series (first, last, interval) =
    if first > last then
      []
    else
      (VAR first) :: (var_series ((first +. interval), last, interval))
  in
  let get_last xs =
    let rev = List.rev xs in
      (List.hd rev, List.rev (List.tl rev))
  in
  let float_of_var x =
    match x with
    UNBINDED -> raise FreeVariable
      | VAR f -> f
  in
  let rec eval (exp, x) =
    match exp with
    X -> (
      match x with
              UNBINDED -> raise FreeVariable
            | VAR v -> v
    )
      | INT i -> float_of_int i
      | REAL r -> r
      | ADD (op1, op2) -> eval (op1, x) +. eval (op2, x)
      | SUB (op1, op2) -> eval (op1, x) -. eval (op2, x)
      | MUL (op1, op2) -> eval (op1, x) *. eval (op2, x)
      | DIV (op1, op2) ->
      let float_of_op2 = eval (op2, x) in
            if float_of_op2 = 0.0 then
              raise DivideByZero
            else
              eval (op1, x) /. float_of_op2

      | SIGMA (f, l, op) ->
          let first = eval (f, x) in
      let last = eval (l, x) in
            if first > last then
              raise InvalidSigma
            else
              List.fold_left (+.) 0.0 (List.map (fun x -> eval (op, x)) (var_series (first, last, 1.0)))

      | INTEGRAL (f, l, op) ->
      let first = eval (f, x) in
      let last = eval (l, x) in
            if first = last then
              0.0
            else if first < last then
              let vs = var_series (first, last, 0.1) in
              let (vn, vs) = get_last vs in
        (List.fold_left (+.) 0.0 (List.map (( *. ) 0.1) (List.map (fun x -> eval (op, x)) vs)))
        +.
          ((last -. (float_of_var vn)) *. (eval (op, vn)))
            else
              (-1.0) *. (eval (INTEGRAL (l, f, op), x))
  in
    eval (exp, UNBINDED)

(*
 * Department  : EE
 * Student No. : 2009-20769
 * Name        : Kim, Seongjun
 * Exercise 2
 *)

let rec zipper (xl, yl) =
    match xl with
        [] -> yl
        | x :: xs -> x :: zipper(yl, xs)

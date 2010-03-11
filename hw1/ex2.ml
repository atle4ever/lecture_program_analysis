(*
 * Department  : EE
 * Student No. : 2009-20769
 * Name        : Kim, Seongjun
 * Exercise 2
 *)

let rec zipper (x, y) =
    match x with
        [] -> y
        | x_head :: x_tail -> x_head :: zipper(y, x_tail)

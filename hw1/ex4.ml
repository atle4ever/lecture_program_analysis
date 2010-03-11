(*
 * Department  : EE
 * Student No. : 2009-20769
 * Name        : Kim, Seongjun
 * Exercise 4
 *)

type nat = ZERO | SUCC of nat

let rec natadd (a, b) =
    match b with
        ZERO -> a
        | SUCC nat -> natadd ((SUCC a), nat)

let rec natmul (a, b) =
    match b with
        ZERO -> ZERO
        | SUCC nat -> natadd (a, (natmul (a, nat)))

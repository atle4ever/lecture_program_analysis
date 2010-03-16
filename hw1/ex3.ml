(*
 * Department  : EE
 * Student No. : 2009-20769
 * Name        : Kim, Seongjun
 * Exercise 3
 *)

let rec zipperN (ll: int list list) =
    let first_list = List.hd ll in
    let remain_lists = List.tl ll in

    if ll = [[]] then []
    else
        match first_list with
            [] -> zipperN remain_lists
            | x :: xs -> x :: zipperN (List.append remain_lists [xs])

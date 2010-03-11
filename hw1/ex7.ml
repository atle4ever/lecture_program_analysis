(*
 * Department  : EE
 * Student No. : 2009-20769
 * Name        : Kim, Seongjun
 * Exercise 7
 *)

type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank = function
    EMPTY -> -1
  | NODE(r,_,_,_) -> r

let findMin = function
    EMPTY -> raise EmptyHeap
  | NODE(_,x,_,_) -> x

let rec merge =
  let shake = function (x,lh,rh) ->
    if (rank lh) >= (rank rh)
    then NODE(rank rh + 1, x, lh, rh)
    else NODE(rank lh + 1, x, rh, lh)
  in

  let left = function
    EMPTY -> raise EmptyHeap
    | NODE(_,x,lh,_) -> lh
  in

  let right = function
      EMPTY -> raise EmptyHeap
    | NODE(_,x,_,rh) -> rh
  in
    function
      EMPTY, rh -> rh
      | lh, EMPTY -> lh
      | lh, rh ->
          let lo = if (findMin lh) < (findMin rh) then lh else rh in
          let hi = if (findMin lh) > (findMin rh) then lh else rh in
            shake(findMin lo, left lo, merge(right lo, hi))

let insert = function (x, h) -> merge(h, NODE(0, x,EMPTY,EMPTY))

let deleteMin = function
    EMPTY -> raise EmptyHeap
  | NODE(_,x,lh,rh) -> merge(lh,rh)


(*
 * Department  : EE
 * Student No. : 2009-20769
 * Name        : Kim, Seongjun
 * Exercise 3
 *)

module type CPO =
  sig
    type t
    val order: t * t -> bool
    val lub: t * t -> t
    val bottom: t
  end

module type FIX =
  sig
    type t
    val fix: (t -> t) -> t
  end

module Fix(D: CPO): FIX with type t = D.t =
struct
  type t = D.t
  let fix  = function (f) ->
    let rec intra_fix = function (f, f_i, prev_lub) ->
      let f_j = f (f_i) in
      let curr_lub = D.lub(prev_lub, f_j) in
        if D.order(curr_lub, prev_lub) then
          curr_lub
        else
          intra_fix (f, f_j, curr_lub)
    in
      intra_fix(f, D.bottom, D.bottom)
end

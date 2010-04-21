type t = INT of int | LOC of string
let string_of_value v = match v with
    INT i -> "INT("^(string_of_int i)^")"
  | LOC l -> "LOC("^l^")"
let compare v1 v2 = match (v1, v2) with
    (INT i1, INT i2) -> i1 = i2
  | (LOC l1, LOC l2) -> l1 = l2
  | _ -> false

module OrderedTypeForString : (Map.OrderedType with type t = string) = 
struct
  type t = string
  let compare = compare
end
module M = Map.Make(OrderedTypeForString)
type t = Value.t M.t
let empty = (M.empty : Value.t M.t)
let bind x v m = M.add x v m
let lookup x m = M.find x m
let equal m1 m2  = M.equal Value.compare m1 m2
let string_of_memory m =
	(if 
		M.is_empty m then "    Empty\n"
	else
		M.fold (fun k v str-> str^"    "^k^" : "^(Value.string_of_value v)^"\n") m "")


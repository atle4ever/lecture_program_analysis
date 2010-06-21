module Parity =
struct
  type t = BOT | EVEN | ODD | TOP

  let bot = BOT
  let top = TOP
  let even = EVEN
  let odd = ODD

  let join prty1 prty2 =
    match prty1, prty2 with
        BOT, _ -> prty2
      | _, BOT -> prty1
      | EVEN, EVEN | ODD, ODD -> prty1
      | _, _ -> TOP

  let eq prty1 prty2 =
    match prty1, prty2 with
        BOT, BOT -> true
      | TOP, TOP -> true
      | EVEN, EVEN -> true
      | ODD, ODD -> true
      | _, _ -> false
    
  let string_of e =
    match e with
        BOT -> "bot"
      | TOP -> "top"
      | EVEN -> "even"
      | ODD -> "odd"
end

module Interval =
struct
  type bound = Z of int | Pinfty | Ninfty
  exception Undefined
  type t = BOT | TOP | ELT of bound * bound

  let bot = BOT
  let top = TOP

  let make l h = ELT (l, h)

  let min_bound b1 b2 =
    match b1, b2 with
        Pinfty, _ -> b2
      | _, Pinfty -> b1
      | Ninfty, _ -> Ninfty
      | _, Ninfty -> Ninfty
      | Z z1, Z z2 -> Z (min z1 z2)
  and max_bound b1 b2 =
    match b1, b2 with
        Ninfty, _ -> b2
      | _, Ninfty -> b1
      | Pinfty, _ -> Pinfty
      | _, Pinfty -> Pinfty
      | Z z1, Z z2 -> Z (max z1 z2)

  let eq_bound bound1 bound2 =
    match bound1, bound2 with
        Z z1, Z z2 -> z1 = z2
      | Pinfty, Pinfty -> true
      | Ninfty, Ninfty -> true
      | _, _ -> false

  let join itv1 itv2 =
    match itv1, itv2 with
        BOT, _ -> itv2
      | _, BOT -> itv1
      | TOP, _ -> TOP
      | _, TOP -> TOP
      | ELT (l1, h1), ELT (l2, h2) ->
          ELT (min_bound l1 l2, max_bound h1 h2)
    
  let eq itv1 itv2 =
    match itv1, itv2 with
        BOT, BOT -> true
      | TOP, TOP -> true
      | ELT (l1, h1), ELT (l2, h2) ->
          eq_bound l1 l2 && eq_bound h1 h2
      | _, _ -> false

  let rec string_of e = 
	match e with
		BOT -> "bot"
	  | TOP -> "[-inf, +inf]"
	  | ELT (l, u) ->
          "[" ^ string_of_bound l ^ ", " ^ string_of_bound u ^ "]"
  and string_of_bound b = 
	match b with 
		Z i -> string_of_int i
	  | Pinfty -> "+inf"
	  | Ninfty -> "-inf"
end

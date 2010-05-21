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

  let make l h = ELT (Z l, Z h)

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


(*****************)
module Bool =
struct
  type t = BOT | TRUE | FALSE | TOP

  let bot = BOT
  let top = TOP
  let t = TRUE
  let f = FALSE

  let join b1 b2 =
    match b1, b2 with
        BOT, _ -> b2
      | _, BOT -> b1
      | TRUE, TRUE | FALSE, FALSE -> b1
      | _, _ -> TOP

  let eq prty1 prty2 =
    match prty1, prty2 with
        BOT, BOT -> true
      | TOP, TOP -> true
      | TRUE, TRUE -> true
      | FALSE, FALSE -> true
      | _, _ -> false

  let string_of e =
    match e with
        BOT -> "bot"
      | TOP -> "top"
      | TRUE -> "true"
      | FALSE -> "false"
end

module Z =
struct
  type t = Parity.t * Interval.t

  let bot = (Parity.bot, Interval.bot)
  let top = (Parity.top, Interval.top)

  let join (p1, i1) (p2, i2) =
    (Parity.join p1 p2, Interval.join i1 i2)

  let eq (p1, i1) (p2, i2) =
    (Parity.eq p1 p2) && (Interval.eq i1 i2)

  let string_of e =
    match e with
        (p, i) -> "(" ^ Parity.string_of p ^ ", " ^ Interval.string_of i ^ ")"
end

module Loc =
struct
  include Set.Make(struct type t = string let compare = compare end)

  let join loc1 loc2 =
    union loc1 loc2

  let eq loc1 loc2 =
    equal loc1 loc2
end

module Val =
struct
  type t = BOT | TOP | Z of Z.t | Bool of Bool.t | Loc of Loc.t

  let bot = BOT

  let top = TOP

  let join v1 v2 =
    match v1, v2 with
        BOT, _ -> v2
      | _, BOT -> v1
      | Z z1, Z z2 -> Z (Z.join z1 z2)
      | Bool b1, Bool b2 -> Bool (Bool.join b1 b2)
      | Loc l1, Loc l2 -> Loc (Loc.join l1 l2)
      | _, _ -> TOP

  let eq v1 v2 =
    match v1, v2 with
        BOT, BOT -> true
      | TOP, TOP -> true
      | Z z1, Z z2 -> Z.eq z1 z2
      | Bool b1, Bool b2 -> Bool.eq b1 b2
      | Loc l1, Loc l2 -> Loc.eq l1 l2
      | _, _ -> false
end

module M =
struct
  include Map.Make(struct type t = string let compare = compare end)

  let bind x v m = add x v m
  let lookup x m = find x m

  let join m1 m2 =
    fold (fun x v1 joined_m ->
              let v2 = try lookup x joined_m with Not_found -> Val.bot in
                bind x (Val.join v1 v2) joined_m
           ) m1 m2

  let equal m1 m2 = equal Val.eq m1 m2
end

module State =
struct
  type t = K.cmd * (Val.t M.t)
end
(*****************)

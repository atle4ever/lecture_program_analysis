open K

module Parity =
struct
  type t = EVEN | ODD | TOP

  let top = TOP
  let even = EVEN
  let odd = ODD

  let convert i = if (i mod 2) == 0 then EVEN else ODD

  let join prty1 prty2 =
    match prty1, prty2 with
        EVEN, EVEN | ODD, ODD -> prty1
      | _, _ -> TOP

  let eq prty1 prty2 =
    match prty1, prty2 with
        TOP, TOP -> true
      | EVEN, EVEN -> true
      | ODD, ODD -> true
      | _, _ -> false

  let string_of e =
    match e with
        TOP -> "top"
      | EVEN -> "even"
      | ODD -> "odd"
end

module Interval =
struct
  type bound = Z of int | Pinfty | Ninfty

  type t = bound * bound

  let top = (Ninfty, Pinfty)
  let make l h =
    if h < l then
      raise (Error "Invalid Z. low > high")
    else
      (Z l, Z h)

  let add_bound b1 b2 =
    match b1, b2 with
        Pinfty, Ninfty | Ninfty, Pinfty -> raise (Error "Invalid operation (Pinfty + Ninfty)")
      | Pinfty, _ -> b1
      | _, Pinfty -> b2
      | Ninfty, _ -> b2
      | _, Ninfty -> b1
      | Z z1, Z z2 -> Z (z1 + z2)

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

  let less_bound b1 b2 =
    match b1, b2 with
        Pinfty, Pinfty | Ninfty, Ninfty -> false
      | _, Pinfty | Ninfty, _ -> true
      | Z i1, Z i2 -> i1 < i2
      | _ -> false

  let join itv1 itv2 =
    match itv1, itv2 with
        (l1, h1), (l2, h2) ->
          (min_bound l1 l2, max_bound h1 h2)

  let widen itv1 itv2 =
    match itv1, itv2 with
        (l1, h1), (l2, h2) ->
          ((if less_bound l2 l1 then Ninfty else l1), (if less_bound l1 h2 then Pinfty else h2))

  let narrow itv1 itv2 =
    match itv1, itv2 with
        (l1, h1), (l2, h2) ->
          ((if l1 == Ninfty then l2 else l1), (if h1 == Pinfty then h2 else h1))

  let eq itv1 itv2 =
    match itv1, itv2 with
        (l1, h1), (l2, h2) ->
          eq_bound l1 l2 && eq_bound h1 h2

  let desc b =
    match b with
        Pinfty | Ninfty -> b
      | Z i -> Z (i-1)

  let incr b =
    match b with
        Pinfty | Ninfty -> b
      | Z i -> Z (i+1)

  let rec string_of e =
    match e with
        (l, u) ->
          "[" ^ string_of_bound l ^ ", " ^ string_of_bound u ^ "]"

  and string_of_bound b =
    match b with
        Z i -> string_of_int i
      | Pinfty -> "+inf"
      | Ninfty -> "-inf"
end

module Bool =
struct
  type t = TRUE | FALSE | TOP

  let top = TOP
  let mt = TRUE
  let mf = FALSE

  let join b1 b2 =
    match b1, b2 with
        TRUE, TRUE | FALSE, FALSE -> b1
      | _, _ -> TOP

  let eq prty1 prty2 =
    match prty1, prty2 with
        TOP, TOP -> true
      | TRUE, TRUE -> true
      | FALSE, FALSE -> true
      | _, _ -> false

  let string_of e =
    match e with
        TOP -> "top"
      | TRUE -> "true"
      | FALSE -> "false"
end

module Z =
struct
  type t = Parity.t * Interval.t

  let join (p1, i1) (p2, i2) =
    (Parity.join p1 p2, Interval.join i1 i2)

  let widen (p1, i1) (p2, i2) =
    (Parity.join p1 p2, Interval.widen i1 i2)

  let narrow (p1, i1) (p2, i2) =
    (Parity.join p1 p2, Interval.narrow i1 i2)

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

  let string_of ls =
    (fold (fun l str -> str ^ l ^ " ") ls "(") ^ ")"
end

module Val =
struct
  type t = TOP | Z of Z.t | Bool of Bool.t | Loc of Loc.t

  let top = TOP

  let z_of : t -> Z.t
    = fun v ->
      match v with
          Z z -> z
        | Bool _ -> raise (Error "Bool is used as Z")
        | Loc _ -> raise (Error "Loc is used as Z")
        | TOP -> raise (Error "TOP is used as Z")

  let b_of : t -> Bool.t
    = fun v ->
      match v with
          Bool b  -> b
        | Z z -> raise (Error "Z is used as Bool")
        | Loc _ -> raise (Error "Loc is used as Bool")
        | TOP -> raise (Error "TOP is used as Bool")

  let l_of : t -> Loc.t
    = fun v ->
      match v with
          Loc l -> l
        | Z _ -> raise (Error "Z is used as Loc")
        | Bool _ -> raise (Error "Bool is used as Loc")
        | TOP -> raise (Error "TOP is used as Loc")

  let join v1 v2 =
    match v1, v2 with
        Z z1, Z z2 -> Z (Z.join z1 z2)
      | Bool b1, Bool b2 -> Bool (Bool.join b1 b2)
      | Loc l1, Loc l2 -> Loc (Loc.join l1 l2)
      | _, _ -> TOP

  let widen v1 v2 =
    match v1, v2 with
        Z z1, Z z2 -> Z (Z.widen z1 z2)
      | Bool b1, Bool b2 -> Bool (Bool.join b1 b2)
      | Loc l1, Loc l2 -> Loc (Loc.join l1 l2)
      | _, _ -> TOP

  let narrow v1 v2 =
    match v1, v2 with
        Z z1, Z z2 -> Z (Z.narrow z1 z2)
      | Bool b1, Bool b2 -> Bool (Bool.join b1 b2)
      | Loc l1, Loc l2 -> Loc (Loc.join l1 l2)
      | _, _ -> TOP

  let eq v1 v2 =
    match v1, v2 with
        TOP, TOP -> true
      | Z z1, Z z2 -> Z.eq z1 z2
      | Bool b1, Bool b2 -> Bool.eq b1 b2
      | Loc l1, Loc l2 -> Loc.eq l1 l2
      | _, _ -> false

  let string_of v =
    match v with
        TOP -> "top"
      | Z z -> "Z[" ^ (Z.string_of z) ^ "]"
      | Bool b -> "B[" ^ (Bool.string_of b) ^ "]"
      | Loc l -> "L[" ^ (Loc.string_of l) ^ "]"
end

module M =
struct
  include Map.Make(struct type t = string let compare = compare end)

  let bind x v m = add x v m
  let lookup x m = find x m
  let join m1 m2 =
    fold (fun x v1 m' ->
            try let v2 = lookup x m' in bind x (Val.join v1 v2) m'
            with Not_found -> bind x v1 m'
         ) m1 m2

  let widen m1 m2 =
    fold (fun x v1 m' ->
            try let v2 = lookup x m' in bind x (Val.widen v1 v2) m'
            with Not_found -> bind x v1 m'
         ) m1 m2

  let narrow m1 m2 =
    fold (fun x v1 m' ->
            try let v2 = lookup x m' in bind x (Val.narrow v1 v2) m'
            with Not_found -> bind x v1 m'
         ) m1 m2

  let eq m1 m2 = equal Val.eq m1 m2

  let string_of m =
    (fold (fun x v str -> str ^ x ^ " -> " ^ (Val.string_of v) ^ ", ") m "{ ") ^ "}"
end

module Cmd =
struct
  type t = cmd

  let compare c1 c2 =
    match c1, c2 with
        (l1, stmt1), (l2, stmt2) -> l2 - l1

  let eq c1 c2 =
    (compare c1 c2) == 0
end

module State =
struct
  type t = cmd * (Val.t M.t)

  let eq s1 s2 =
    match s1, s2 with
        (c1, m1), (c2, m2) -> (Cmd.eq c1 c2) & (M.eq m1 m2)
end

module StateMap =
struct
  include Map.Make(struct type t = cmd let compare = Cmd.compare end)

  let join sm1 sm2 =
    fold (fun c m sm ->
            let m2 =
              try find c sm
              with Not_found -> M.empty
            in
              add c (M.join m m2) sm
         ) sm1 sm2

  let widen sm1 sm2 =
    fold (fun c m sm ->
            try let m2 = find c sm in add c (M.widen m m2) sm
            with Not_found -> add c m sm
         ) sm1 sm2

  let narrow sm1 sm2 =
    fold (fun c m sm ->
            try let m2 = find c sm in add c (M.narrow m m2) sm
            with Not_found -> add c m sm
         ) sm1 sm2

  let singleton c m =
    add c m empty

  let eq sm1 sm2 =
    equal M.eq sm1 sm2

end
(*****************)

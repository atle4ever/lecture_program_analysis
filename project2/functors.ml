(* set functors for
 * SNU 4541.664A Program Analysis
 * Kwangkeun Yi, 2010
 *)

open Domain

module type SET = sig
  include Set.S
  exception TooMany
  val all: unit -> t
end

module PrimitiveSet (A: sig
                       type t val compare: t -> t -> int
                              exception TooMany
                              val all: unit -> t list
                     end) =
struct
  include Set.Make (A)
  exception TooMany
  let all = fun () -> try
    List.fold_left (fun s x -> add x s) empty (A.all())
  with A.TooMany -> raise TooMany
end

module ProductSet (A: SET) (B: SET) =
struct
  include Set.Make (struct type t = A.elt * B.elt let compare = compare end)
  exception TooMany
  let all = fun () -> try
    A.fold (fun a c ->
              B.fold (fun b c -> add (a, b) c)
                (B.all()) c
           ) (A.all()) empty
  with A.TooMany -> raise TooMany
    | B.TooMany -> raise TooMany
end

module PowerSet (A: SET) =
struct
  include Set.Make (struct type t = A.t let compare = compare end)
  exception TooMany
  let all = fun () -> raise TooMany
end

module FunctionSet (A: SET) (B: SET) =
struct
  module F = Map.Make (struct type t = A.elt let compare = compare end)
  include Set.Make (struct type t = B.elt F.t let compare = compare end)
  exception TooMany
  let all = fun () -> raise TooMany
  let domain_all = A.all
  let range_all = B.all
end

(* domain functors for
 * SNU 4541.664A Program Analysis
 * Kwangkeun Yi, 2010
 *)

module type DOMAIN =
sig
  type elt      (* the type of abstract domain elements *)
  val top: elt
  val bot: elt
  val join: elt -> elt -> elt
  val leq: elt -> elt -> bool
  val widen: elt -> elt -> elt
  val narrow: elt -> elt -> elt
end

module type FLAT_DOMAIN =
sig
  include DOMAIN
  type atom
  val make: atom -> elt
end

module type INT_FLAT_DOMAIN = FLAT_DOMAIN with type atom = int

module type ADDABLE_FLAT_DOMAIN =
sig
  include INT_FLAT_DOMAIN
  val add: elt -> elt -> elt
  val minus: elt -> elt
  val mul: elt -> elt -> elt
  val to_reminder: elt -> Reminder.t
  val is_davinci: elt -> bool
end

module type PRODUCT_DOMAIN =
sig
  include DOMAIN
  type lelt
  type relt
  val l: elt -> lelt  (* left *)
  val r: elt -> relt  (* right *)
  val make: lelt -> relt -> elt
end

module type PRODUCT3_DOMAIN =
sig
  include DOMAIN
  type lelt
  type melt
  type relt
  val l: elt -> lelt  (* left *)
  val m: elt -> melt  (* middle *)
  val r: elt -> relt  (* right *)
  val make: lelt -> melt -> relt -> elt
end

module type POWERSET_DOMAIN =
sig
  include DOMAIN
  type atom
  val union: elt -> elt -> elt
  val inter: elt -> elt -> elt
  val diff: elt -> elt -> elt
  val remove: atom -> elt -> elt
  val mem: atom -> elt -> bool
  val map: (atom -> atom) -> elt -> elt
  val fold: (atom -> 'a -> 'a) -> elt -> 'a -> 'a
  val make: atom list -> elt
  val cardinal: elt -> int
end

module type FUNCTION_DOMAIN =
sig
  include DOMAIN
  type lelt
  type relt
  val image: elt -> lelt -> relt
  val update: elt -> lelt -> relt -> elt
  val map: (lelt -> relt -> lelt * relt) -> elt -> elt
  val fold: (lelt -> relt -> 'a -> 'a) -> elt -> 'a -> 'a
  val make: (lelt * relt) list -> elt
  val duplicate: elt -> elt
end

module type INTERVAL_DOMAIN =
sig
  include DOMAIN
  exception Undefined
  type bound = Z of int | Pinfty | Ninfty
  val l: elt -> bound  (* lower bound *)
  val u: elt -> bound  (* upper bound *)
  val make: bound -> bound -> elt
  val const: int -> elt
  val add: elt -> elt -> elt
  val minus: elt -> elt
  val mul: elt -> elt -> elt
  val negate: bool -> elt -> elt
  val prune: elt -> elt -> bool -> elt * elt
  val intersect: elt -> elt -> elt * elt
  val to_interval: elt -> Interval.t
end

module FlatDomain (A: SET) : FLAT_DOMAIN with type atom = A.elt =
struct
  type elt = BOT | TOP | ELT of A.elt
  type atom = A.elt
  let bot = BOT
  let top = TOP
  let join x y = match (x, y)
  with (BOT, _) -> y
    | (_, BOT) -> x
    | (TOP, _) -> TOP
    | (_, TOP) -> TOP
    | (x, y) -> if x = y then x else TOP
  let leq x y = match (x, y)
  with (BOT, _) -> true
    | (_, TOP) -> true
    | (ELT a, ELT b) -> a = b
    | _ -> false
  let make a = ELT a
  let widen x y = join x y
  let narrow x y = match (x, y)
  with (TOP, _) -> y
    | (_, TOP) -> x
    | _ -> BOT
end

module ProductDomain (A: DOMAIN) (B: DOMAIN) : PRODUCT_DOMAIN
  with type lelt = A.elt and type relt = B.elt =
struct
  type elt = BOT | TOP | ELT of A.elt * B.elt
  type lelt = A.elt
  type relt = B.elt
  let bot = BOT
  let top = TOP
  let join x y = match (x, y)
  with (BOT, _) -> y
    | (TOP, _) -> TOP
    | (_, BOT) -> x
    | (_, TOP) -> TOP
    | (ELT(a, b), ELT(a', b')) -> ELT (A.join a a', B.join b b')
  let leq x y = match (x, y)
  with (BOT, _) -> true
    | (_, BOT) -> false
    | (_, TOP) -> true
    | (TOP, _) -> false
    | (ELT(a, b), ELT(a', b')) -> A.leq a a' && B.leq b b'
  let l x = match x with TOP -> A.top | BOT -> A.bot | ELT (a, b) -> a
  let r x = match x with TOP -> B.top | BOT -> B.bot | ELT (a, b) -> b
  let make a b = ELT (a, b)
  let widen a b = ELT (A.widen (l a) (l b), B.widen (r a) (r b))
  let narrow a b = ELT (A.narrow (l a) (l b), B.narrow (r a) (r b))
end

module Product3Domain (A: DOMAIN) (B: DOMAIN) (C: DOMAIN): PRODUCT3_DOMAIN
  with type lelt = A.elt and type melt = B.elt and type relt = C.elt =
struct
  type elt = BOT | TOP | ELT of A.elt * B.elt * C.elt
  type lelt = A.elt
  type melt = B.elt
  type relt = C.elt
  let bot = BOT
  let top = TOP
  let join x y = match (x, y)
  with (BOT, _) -> y
    | (TOP, _) -> TOP
    | (_, BOT) -> x
    | (_, TOP) -> TOP
    | (ELT(a, b, c), ELT(a', b', c')) -> ELT (A.join a a', B.join b b', C.join c c')
  let leq x y = match (x, y)
  with (BOT, _) -> true
    | (_, BOT) -> false
    | (_, TOP) -> true
    | (TOP, _) -> false
    | (ELT(a, b, c), ELT(a', b', c')) -> A.leq a a' && B.leq b b' && C.leq c c'
  let l x = match x with TOP -> A.top | BOT -> A.bot | ELT (a, b, c) -> a
  let m x = match x with TOP -> B.top | BOT -> B.bot | ELT (a, b, c) -> b
  let r x = match x with TOP -> C.top | BOT -> C.bot | ELT (a, b, c) -> c

  let make a b c = ELT (a, b, c)
  let widen a b = ELT (A.widen (l a) (l b), B.widen (m a) (m b), C.widen (r a) (r b))
  let narrow a b = ELT (A.narrow (l a) (l b), B.narrow (m a) (m b), C.widen (r a) (r b))
end

module PowersetDomain (A: SET) : POWERSET_DOMAIN with type atom = A.elt =
struct
  type elt = BOT | TOP | ELT of A.t
  type atom = A.elt
  let bot = BOT
  let top = TOP
  let join x y = match (x, y)
  with (BOT, _) -> y
    | (_, BOT) -> x
    | (TOP, _) -> TOP
    | (_, TOP) -> TOP
    | (ELT s, ELT s') -> ELT (A.union s s')
  let mem a s = match s with BOT -> false
    | TOP -> true
    | ELT s -> A.mem a s
  let fold f x a = match x with BOT -> a
    | TOP -> A.fold f (A.all()) a
    | ELT s -> A.fold f s a
  let map f x = match x with BOT -> BOT
    | TOP ->
        ELT (A.fold (fun a s -> A.add (f a) s) (A.all()) A.empty)
    | ELT s ->
        ELT (A.fold (fun a s' -> A.add (f a) s') s A.empty)

  let make lst = match lst
  with [] -> BOT
    | l -> ELT
        (List.fold_left (fun s x -> A.add x s)
           A.empty l
        )
  let cardinal x = match x with BOT -> 0
    | TOP -> A.cardinal (A.all())
    | ELT s -> A.cardinal s

(* power set domain for
 * SNU 4541.664A Program Analysis
 * Wonchan Lee, 2010
 *)

  let leq x y = match (x, y)
  with (BOT, _) -> true
    | (_, BOT) -> false
    | (_, TOP) -> true
    | (TOP, _) -> false
    | (ELT s, ELT s') -> A.subset s s'

  let union x y = join x y
  let inter x y = match (x, y)
  with (BOT, _)
    | (_, BOT) -> BOT
    | (TOP, _) -> y
    | (_, TOP) -> x
    | (ELT s, ELT s') -> ELT (A.inter s s')
  let diff x y = match (x, y)
  with (BOT, _) -> BOT
    | (_, BOT) -> x
    | (_, TOP) -> BOT
    | (TOP, ELT s) -> ELT (A.diff (A.all()) s)
    | (ELT s, ELT s') -> ELT (A.diff s s')
  let remove a x = match x with BOT -> BOT
    | TOP -> ELT (A.remove a (A.all()))
    | ELT s -> ELT (A.remove a s)
  let widen x y = join x y
  let narrow x y = inter x y
(* power set domain ends *)
end

module FunDomain (A: SET) (B: DOMAIN) : FUNCTION_DOMAIN
  with type lelt = A.elt and type relt = B.elt
  =
struct
  module Map = Map.Make(struct type t = A.elt let compare = compare end)
  type elt = BOT | TOP | ELT of B.elt Map.t
  type lelt = A.elt
  type relt = B.elt
  let bot = BOT
  let top = TOP

(* function domain for
 * SNU 4541.664A Program Analysis
 * Wonchan Lee, 2010
 *)

(* x: elt, a: key, b: value, c: others *)
  let image x a = match x with BOT -> B.bot
    | TOP -> B.top
    | ELT s ->
        if Map.mem a s
        then Map.find a s
        else B.bot
  let update x a b = match x with BOT -> ELT (Map.add a b Map.empty)
    | TOP -> ELT (
        Map.add a b
          (A.fold
             (fun a x -> Map.add a B.top x)
             (A.all()) Map.empty
          ))
    | ELT s ->
        ELT (Map.add a b s)
  let map f x = match x with BOT -> BOT
    | TOP -> ELT (
        A.fold
          (fun a x ->
             let (a', b') = f a B.top in
               Map.add a' b' x
          ) (A.all()) Map.empty
      )
    | ELT s -> ELT (
        Map.fold
          (fun a b x ->
             let (a', b') = f a b in
               Map.add a' b' x
          )
          s Map.empty
      )
  let fold f x c = match x with BOT -> c
    | TOP -> A.fold
        (fun a c -> f a B.top c)
          (A.all()) c
    | ELT s -> Map.fold f s c
  let make pairs = match pairs with [] -> BOT
    | _ -> ELT (List.fold_right
                  (fun (a, b) x -> Map.add a b x)
                  pairs Map.empty
               )

  let join x y = match (x, y)
  with (BOT, _) -> y
    | (_, BOT) -> x
    | (TOP, _) -> TOP
    | (_, TOP) -> TOP
    | (ELT _, ELT s') -> Map.fold
        (fun a b x ->
           update x a (B.join b (image x a)))
          s' x
  let leq x y = match (x, y)
  with (BOT, _) -> true
    | (_, BOT) -> false
    | (_, TOP) -> true
    | (TOP, _) -> false
    | (ELT s, ELT s') ->
        let (flag, remain) =
          Map.fold
            (fun a b (flag, remain) ->
               (B.leq (image x a) (image y a) && flag,
                Map.remove a remain))
            s'
            (true, s)
        in
          flag && Map.is_empty remain

  let duplicate x = let id a b = (a, b) in map id x

  let widen x y =
    fold (fun a b y -> update y a (B.widen b (image y a))) x y

  let narrow x y =
    fold (fun a b y -> update y a (B.narrow b (image y a))) x y

(* function domain ends *)
end

module Zintvl : INTERVAL_DOMAIN =
struct
  type bound = Z of int | Pinfty | Ninfty
  exception Undefined
  type elt = BOT | TOP | ELT of bound * bound

  let top = TOP
  let bot = BOT

  let min_bound b1 b2 = match (b1, b2)
  with (Pinfty, _) -> b2
    | (_, Pinfty) -> b1
    | (Ninfty, _) -> Ninfty
    | (_, Ninfty) -> Ninfty
    | (Z z1, Z z2) -> Z (min z1 z2)
  and max_bound b1 b2 = match (b1, b2)
  with (Ninfty, _) -> b2
    | (_, Ninfty) -> b1
    | (Pinfty, _) -> Pinfty
    | (_, Pinfty) -> Pinfty
    | (Z z1, Z z2) -> Z (max z1 z2)

  let bleq b1 b2 = match (b1, b2)
  with (_, Pinfty) -> true
    | (Ninfty, _) -> true
    | (Pinfty, _) -> false
    | (_, Ninfty) -> false
    | (Z z1, Z z2) -> z1 <= z2

  let join itv1 itv2 = match (itv1, itv2)
  with (BOT, _) -> itv2
      | (_, BOT) -> itv1
      | (TOP, _) -> TOP
      | (_, TOP) -> TOP
      | (ELT (l1, h1), ELT (l2, h2)) ->
          ELT (min_bound l1 l2, max_bound h1 h2)

  let leq itv1 itv2 = match (itv1, itv2)
  with (BOT, _) -> true
    | (_, TOP) -> true
    | (_, BOT) -> false
    | (TOP, _) -> false
    | (ELT (l1, h1), ELT (l2, h2)) ->
        bleq l2 l1 && bleq h1 h2

  let l itv = match itv with TOP -> Ninfty
    | BOT -> raise Undefined
    | ELT (l, _) -> l
  let u itv = match itv with TOP -> Pinfty
    | BOT -> raise Undefined
    | ELT (_, h) -> h
  let make l h = ELT (l, h)
  let const n = make (Z n) (Z n)

  let add_bound b1 b2 = match (b1, b2)
  with (Pinfty, _)
    | (_, Pinfty) -> Pinfty
    | (Ninfty, _)
    | (_, Ninfty) -> Ninfty
    | (Z z1, Z z2) -> Z (z1 + z2)

  let add itv1 itv2 = match (itv1, itv2)
  with (TOP, _)
    | (_, TOP) -> TOP
    | (ELT (l1, h1), ELT (l2, h2)) ->
        make (add_bound l1 l2) (add_bound h1 h2)
    | _ -> BOT

  let minus_bound b = match b with Pinfty -> Ninfty
    | Ninfty -> Pinfty
    | Z z -> Z (-z)

  let minus itv = match itv with TOP -> TOP
    | ELT (l, h) ->
        make (minus_bound h) (minus_bound l)
    | _ -> BOT

  let mul_bound b1 b2 = match (b1, b2)
  with (Pinfty, _)
    | (_, Pinfty) -> Pinfty
    | (Ninfty, _)
    | (_, Ninfty) -> Ninfty
    | (Z z1, Z z2) -> Z (z1 * z2)

  let mul itv1 itv2 = match (itv1, itv2)
  with (TOP, _)
    | (_, TOP) -> TOP
    | (ELT (l1, h1), ELT (l2, h2)) ->
        let bs = [mul_bound l1 l2; mul_bound l1 h2; mul_bound h1 l2; mul_bound h1 h2] in
        let min = List.fold_left (fun m b -> if bleq b m then b else m) Pinfty bs in
        let max = List.fold_left (fun m b -> if bleq b m then b else m) Ninfty bs in
          ELT (min, max)
    | _ -> BOT


  let widen itv1 itv2 = match (itv1, itv2)
  with (TOP, _)
    | (_, TOP) -> TOP
    | (BOT, _) -> itv2
    | (_, BOT) -> itv1
    | (ELT (l1, h1), ELT (l2, h2)) ->
        make
          (if bleq l1 l2 then l1 else Ninfty)
          (if bleq h2 h1 then h1 else Pinfty)

  let narrow itv1 itv2 = match (itv1, itv2)
  with (TOP, _) -> itv2
    | (_, TOP) -> itv1
    | (BOT, _) -> BOT
    | (_, BOT) -> BOT
    | (ELT (l1, h1), ELT (l2, h2)) ->
        make
          (if l1 = Ninfty then l2 else min_bound l1 l2)
          (if h1 = Pinfty then h2 else max_bound h1 h2)

  let negate sign itv =
    if sign then itv else minus itv

  let expand itv = match itv with TOP -> ELT (Ninfty, Pinfty)
    | _ -> itv

  let correct itv = match itv
  with (ELT (l, h)) ->
    if not (bleq l h) then BOT else itv
    | _ -> itv

  (* returns intervals pruned to satisfy
   * itv1 < itv2 when inclusive is false and
   * itv1 <= itv2 when inclusive is true
   *)
  let prune itv1 itv2 inclusive = match (itv1, itv2)
  with (BOT, _)
    | (_, BOT) -> (itv1, itv2)
    | _ ->
        let itv1' = expand itv1 in
        let itv2' = expand itv2 in
          match (itv1', itv2') with
              (ELT (l1, h1), ELT (l2, h2)) ->
                let h2' = if inclusive then h2 else add_bound h2 (Z (-1)) in
                let l1' = if inclusive then l1 else add_bound l1 (Z 1) in
                let itv1'' = correct (ELT (l1, min_bound h2' h1)) in
                let itv2'' = correct (ELT (max_bound l1' l2, h2)) in
                  (itv1'', itv2'')
            | _ -> raise (Failure "prunning error")

  let intersect itv1 itv2 = match (itv1, itv2)
  with (BOT, _)
    | (_, BOT) -> (itv1, itv2)
    | _ ->
        let itv1' = expand itv1 in
        let itv2' = expand itv2 in
          match (itv1', itv2') with
              (ELT (l1, h1), ELT (l2, h2)) ->
                let itv1 = correct (ELT (max_bound l1 l2, min_bound h1 h2)) in
                let itv2_low = correct (ELT (l1, min_bound h1 l2)) in
                let itv2_high = correct (ELT (max_bound h2 l1, h1)) in
                  (itv1, join itv2_low itv2_high)
            | _ -> raise (Failure "prunning error")

  let to_interval : elt -> Interval.t
    = fun itv -> match itv with BOT -> Interval.BOT
      | TOP -> Interval.TOP
      | ELT _ ->
          let l = l itv in
          let u = u itv in
          let convert_bound b =
            match b with
                Pinfty -> Interval.Pinfty
              | Ninfty -> Interval.Ninfty
              | Z n -> Interval.Z n
          in
            Interval.make
              (convert_bound l)
              (convert_bound u)

end

module Rem : ADDABLE_FLAT_DOMAIN =
struct
  type elt = TOP | BOT | ELT of int
  type atom = int

  let to_reminder x = match x with BOT -> Reminder.BOT
    | TOP -> Reminder.TOP
    | ELT i -> Reminder.ELT i

  let top = TOP
  let bot = BOT
  let join x y = match (x, y)
  with (BOT, _) -> y
    | (_, BOT) -> x
    | (TOP, _) -> TOP
    | (_, TOP) -> TOP
    | _ -> if x = y then x else TOP
  let leq x y = match (x, y)
  with (BOT, _) -> true
    | (_, BOT) -> false
    | (_, TOP) -> true
    | (TOP, _) -> false
    | _ -> if x = y then true else false
  let widen x y = join x y
  let narrow x y = match (x, y)
  with (TOP, _) -> y
    | (_, TOP) -> x
    | (BOT, _) -> BOT
    | (_, BOT) -> BOT
    | _ -> if x = y then x else BOT

  let rem a b =
    let m = a mod b in
      if (a * b) > 0 then m else m + b

  let make n = ELT (rem n 1867)

  let add x y = match (x, y)
  with (BOT, _) -> BOT
    | (_, BOT) -> BOT
    | (TOP, _) -> TOP
    | (_, TOP) -> TOP
    | ELT a, ELT b -> ELT (rem (a+b) 1867)

  let minus x = match x
  with ELT r -> ELT (1867 - r)
    | _ -> x

  let mul x y = match (x, y)
  with (BOT, _) -> BOT
    | (_, BOT) -> BOT
    | (TOP, _) -> TOP
    | (_, TOP) -> TOP
    | ELT a, ELT b -> ELT (rem (a*b) 1867)

  let is_davinci x = match x
  with ELT r -> r == 415
    | _ -> false

end

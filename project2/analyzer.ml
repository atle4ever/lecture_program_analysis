(* Analyzer code *)
open K

(* From K module *)
let rec vars_of_cmd (l, pgm) = match pgm with
    ASSIGN (v, _)
  | ASSIGNSTAR (v, _) -> [v]
  | SEQ (c1, c2)
  | IF (_, c1, c2) -> vars_of_cmd c1 @ vars_of_cmd c2
  | WHILE (_, c) -> vars_of_cmd c
  | _ -> []

let rec exp_to_addlist e sign = match e with
    ADD (e1, e2) -> exp_to_addlist e1 sign @ exp_to_addlist e2 sign
  | MINUS e -> exp_to_addlist e (not sign)
  | LESS (e1, e2) -> exp_to_addlist e1 sign @ exp_to_addlist e2 (not sign)
  | _ -> [(e, sign)]
(* -- *)

(* From Domain module *)
module Reminder =
struct
  type t = BOT | TOP | ELT of int

  let bot = BOT
  let top = TOP

  let string_of e =
    match e with
        BOT -> "bot"
      | TOP -> "top"
      | ELT i -> "rem(" ^ (string_of_int i) ^ ")"
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
(* -- *)

(* From Functor module *)
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
  val isZero: elt -> bool
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
  with (Z 0, _) | (_, Z 0) -> Z 0
    | (Pinfty, _)
    | (_, Pinfty) -> Pinfty
    | (Ninfty, _)
    | (_, Ninfty) -> Ninfty
    | (Z z1, Z z2) -> Z (z1 * z2)

  let rec mul itv1 itv2 = match (itv1, itv2)
  with (TOP, _) -> mul (ELT (Ninfty, Pinfty)) itv2
    | (_, TOP) -> mul itv1 (ELT (Ninfty, Pinfty))
    | (ELT (l1, h1), ELT (l2, h2)) ->
        let bs = [mul_bound l1 l2; mul_bound l1 h2; mul_bound h1 l2; mul_bound h1 h2] in
        let min = List.fold_left (fun m b -> if bleq b m then b else m) Pinfty bs in
        let max = List.fold_left (fun m b -> if bleq m b then b else m) Ninfty bs in
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

  let isZero itv = match itv
  with ELT (Z 0, Z 0) -> true
    | _ -> false

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
      if (a * b) >= 0 then m else m + b

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
(* -- *)

module type ANALYZER =
sig
  type state
  type solution

  val n : program -> label -> cmd
  val analyze : program -> solution
  val get_all_davinci_vars : solution -> id list
end

module type ID_SET = SET with type elt = id
module type INT_SET = SET with type elt = int

module LocDomain (Loc: ID_SET) = PowersetDomain (Loc)
module Fact = ProductDomain (Zintvl) (Rem)
module Value (Loc: ID_SET) = ProductDomain (LocDomain (Loc)) (Fact)
module Memory (Loc: ID_SET) = FunDomain (Loc) (Value (Loc))

module Interpreter (Loc: ID_SET) =
struct
  module LocDomain = LocDomain (Loc)
  module Value = Value (Loc)
  module State = Memory (Loc)

  type state = State.elt
  type value = Value.elt
  type fact = Fact.elt
  type locs = LocDomain.elt
  type interval = Zintvl.elt
  type parity = Rem.elt

  type prty = EVEN | ODD

  let make_fact : locs -> interval -> parity -> value
    = fun loc itv prty -> Value.make loc (Fact.make itv prty)

  let get_locs : value -> locs
    = fun v -> Value.l v

  let get_fact : value -> fact
    = fun v -> Value.r v

  let get_interval : value -> interval
    = fun v -> Fact.l (Value.r v)

  let get_parity : value -> parity
    = fun v -> Fact.r (Value.r v)

  let fact_to_pair : fact -> interval * parity
    = fun f -> (Fact.l f, Fact.r f)

  let pair_to_fact : interval * parity -> fact
    = fun (itv, prty) -> Fact.make itv prty


  let rec next : program -> cmd -> state -> state
    = fun pgm (l, cmd) s ->
      match cmd with
          ASSIGN (x, e) ->
            State.update s x (eval e s)
        | ASSIGNSTAR (x, e) ->
            let locs = get_locs (State.image s x) in
            let v = eval e s in
              if LocDomain.cardinal locs = 1
              then
                (* strong update if a variable refers to only one variable *)
                LocDomain.fold
                  (fun x' s' -> State.update s' x' v)
                  locs s
              else
                (* weak update if a variable refers to more than one variable *)
                LocDomain.fold
                  (fun x' s' ->
                      let v' = State.image s' x' in
                        State.update s' x' (Value.join v v'))
                  locs s
        | _ -> s
  and eval : exp -> state -> value
  = fun e s ->
    match e with
        NUM n ->
          make_fact LocDomain.bot (Zintvl.const n) (Rem.make n)
      | ADD (e1, e2) ->
          let v1 = eval e1 s in
          let v2 = eval e2 s in
          let itv1 = get_interval v1 in
          let itv2 = get_interval v2 in
          let prty1 = get_parity v1 in
          let prty2 = get_parity v2 in
            make_fact
              LocDomain.bot
              (Zintvl.add itv1 itv2)
              (Rem.add prty1 prty2)
      | MINUS e ->
          let v = eval e s in
          let itv = get_interval v in
          let prty = get_parity v in
            make_fact
              LocDomain.bot
              (Zintvl.minus itv)
              (Rem.minus prty)
      | MUL (e1, e2) ->
          let v1 = eval e1 s in
          let v2 = eval e2 s in
          let itv1 = get_interval v1 in
          let itv2 = get_interval v2 in
          let prty1 = get_parity v1 in
          let prty2 = get_parity v2 in
            if Zintvl.isZero itv1 or Zintvl.isZero itv2
            then
              make_fact
                LocDomain.bot
                (Zintvl.make (Zintvl.Z 0) (Zintvl.Z 0))
                (Rem.make 0)
            else
              make_fact
                LocDomain.bot
                (Zintvl.mul itv1 itv2)
                (Rem.mul prty1 prty2)

      | VAR x -> State.image s x
      | STAR x ->
          let locs = get_locs (State.image s x) in
            LocDomain.fold
              (fun x' v -> Value.join v (State.image s x'))
              locs Value.bot
      | AMPER x ->
          make_fact
            (LocDomain.make [x])
            Zintvl.bot
            Rem.bot
      | READ ->
          make_fact LocDomain.bot Zintvl.top Rem.top
      | _ -> make_fact LocDomain.bot Zintvl.bot Rem.bot

  let add_all : (exp * bool) list -> state -> interval * parity
    = fun ls s ->
      List.fold_right
        (fun (e, sign) (sum, prty) ->
           let v = eval e s in
           let itv = get_interval v in
           let prty' = get_parity v in
             (Zintvl.add sum (Zintvl.negate sign itv),
              Rem.add prty prty')
        )
        ls (Zintvl.const 0, Rem.make 0)

  let rec check_mul : exp -> bool
    = fun e ->
      match e with
          ADD (e1, e2) | LESS (e1, e2) -> check_mul(e1) && check_mul(e2)
        | MINUS e1 -> check_mul(e1)
        | NUM _ | TRUE | FALSE | VAR _ | STAR _ | AMPER _ | READ -> false
        | MUL _ -> true

  let prune : exp -> state -> state * state
    = fun e s ->
      (* utility function to generate parity value from interval *)
      let interval_to_parity : interval -> parity
        = fun itv ->
          try
            let l = Zintvl.l itv in
            let u = Zintvl.u itv in
              match (l, u) with
                  (Zintvl.Ninfty, _) -> Rem.top
                | (_, Zintvl.Pinfty) -> Rem.top
                | (Zintvl.Z z1, Zintvl.Z z2) ->
                    if z1 <> z2 then Rem.top else Rem.make z1
                | _ -> raise (Failure "invalid interval")
          with
              Zintvl.Undefined -> Rem.bot
      in
      let exp_is_less = (match e with LESS _ -> true | _ -> false) in
      let with_mul = check_mul e in
      let all = exp_to_addlist e true in
      let s_true = State.duplicate s in
      let s_false = State.duplicate s in
      let prune_itv : interval -> interval -> bool -> interval * interval
        = fun itv1 itv2 exp_is_less ->
          if exp_is_less then
            (fst (Zintvl.prune itv1 itv2 false),
             snd (Zintvl.prune itv2 itv1 true))
          else
            Zintvl.intersect itv1 itv2
      in
      let update_if_not_bot : interval -> parity -> state -> id -> state
        = fun itv prty s x ->
          if itv = Zintvl.bot
          then State.bot
          else State.update s x (make_fact LocDomain.bot itv prty)
      in
        if with_mul then
          (s_true, s_false)
        else (
          List.fold_left
            (fun (s_true, s_false) ((e, sign) as elem) ->
               match e with
                   VAR x ->
                     let all_except_x =
                       List.filter (fun elem' -> not (elem == elem')) all in
                     let (itv1, _) = add_all [(e, sign)] s in
                     let (itv2, _) = add_all all_except_x s in
                     let itv2' = Zintvl.minus itv2 in
                     let (itv1_true, itv1_false) =
                       prune_itv itv1 itv2' exp_is_less in
                     let itv1_true' = Zintvl.negate sign itv1_true in
                     let itv1_false' = Zintvl.negate sign itv1_false in
                     let prty1_true = interval_to_parity itv1_true in
                     let prty1_false = interval_to_parity itv1_false in
                     let s_true' =
                       update_if_not_bot itv1_true' prty1_true s_true x in
                     let s_false' =
                       update_if_not_bot itv1_false' prty1_false s_false x in
                       (s_true', s_false')
                 | TRUE -> (s_true, State.bot)
                 | FALSE -> (State.bot, s_false)
                 | _ -> (s_true, s_false)
            )
            (s_true, s_false) all
        )

  let get_parity_of : string -> state -> parity
    = fun x s -> get_parity (State.image s x)

  let get_interval_of : string -> state -> interval
    = fun x s -> get_interval (State.image s x)

  let string_of_value : value -> string
    = fun v ->
      let itv = get_interval v in
      let prty = get_parity v in
        Interval.string_of (Zintvl.to_interval itv) ^ ", " ^
          Reminder.string_of (Rem.to_reminder prty)

  let string_of_state : state -> string
    = fun s ->
      State.fold
        (fun x f s ->
           s ^ x ^ ":" ^ string_of_value f ^ "\n")
        s ""
end


module Analyzer =
struct
  module Loc = PrimitiveSet (struct type t = string exception TooMany let compare = compare let all = fun () -> raise TooMany end)
  module Label = PrimitiveSet (struct type t = int exception TooMany let compare = compare let all = fun () -> raise TooMany end)
  module Value = Value (Loc)
  module LocDomain = LocDomain (Loc)
  module State = Memory (Loc)
  module Solution = FunDomain (Label) (State)

  module Inter = Interpreter (Loc)

  type value = Value.elt
  type fact = Fact.elt
  type parity = Rem.elt
  type interval = Zintvl.elt
  type state = State.elt
  type solution = Solution.elt
  type operator = solution -> solution -> solution * solution
  type check = solution -> solution -> bool

  let get_fact : value -> fact
    = fun v -> Value.r v

  let get_parity : value -> parity
    = fun v -> Fact.r (Value.r v)

  let get_some : 'a option -> 'a
    = fun a ->
      match a with
          Some a' -> a'
        | None -> raise (Failure "get value of none")

  let print_solution : solution -> unit
    = fun sol ->
      let str =
        Solution.fold
          (fun l s str ->
             str ^ "label " ^ string_of_int l ^ ":\n" ^
               Inter.string_of_state s ^ "\n")
          sol ""
      in
        print_endline str

  (*
   * Returns next command of the given label.
   * All given labels are assumed to be valid.
   * If there is no command left, END is returned.
   *)
  let n : program -> label -> cmd
    = fun pgm target ->
      let rec find : cmd -> cmd list -> cmd list
        = fun ((l, c) as cmd) stack ->
          if target = l then stack
          else
            match c with
                SKIP | ASSIGN _ | ASSIGNSTAR _ | END -> []
              | SEQ (c1, c2) -> find c1 (cmd::stack) @ find c2 stack
              | IF (_, c1, c2) -> find c1 stack @ find c2 stack
              | WHILE (_, c1) -> find c1 (cmd::stack)
      in
      let rec get_next : cmd list -> cmd
        = fun cs ->
          match cs with
              [] -> (-1, END)
            | ((_, SEQ (c1, c2)))::cs -> c2
            | ((_, WHILE (_, _)) as c)::cs -> c
            | _::cs -> get_next cs
      in
        get_next (find pgm [])

  (*
   * Returns current command of the given label.
   *)
  let rec cmd_of_label : program -> label -> cmd option
    = fun (l, c) target ->
      if target = -1
      then Some (-1, END)
      else if l = target
      then Some (l, c)
      else
        match c with
            SEQ (c1, c2)
          | IF (_, c1, c2) ->
              let o1 = cmd_of_label c1 target in
              let o2 = cmd_of_label c2 target in
                (match o1 with
                     Some _ -> o1
                   | _ -> o2)
          | WHILE (_, c) -> cmd_of_label c target
          | _ -> None

  let update : solution -> cmd -> state -> solution
    = fun sol (l, c) s ->
      let s_org = Solution.image sol l in
      let joined = State.join s s_org in
        Solution.update sol l joined

  let next : program -> solution -> solution
    = fun pgm t ->
      Solution.fold
        (fun l s sol ->
           let c = get_some (cmd_of_label pgm l) in
           let c' = n pgm l in
           let s' = Inter.next pgm c s in
             match c with
                 (_, IF (e, c1, c2)) ->
                   let (s_true, s_false) = Inter.prune e s' in
                   let sol_true =
                     if s_true <> State.bot
                     then update sol c1 s_true else sol
                   in
                   let sol_false =
                     if s_false <> State.bot
                     then update sol c2 s_false else sol
                   in
                     Solution.join sol_true sol_false
               | (_, WHILE (e, c'')) ->
                   let (s_true, s_false) = Inter.prune e s' in
                   let sol_true =
                     if s_true <> State.bot
                     then update sol c'' s_true else sol
                   in
                   let sol_false =
                     if s_false <> State.bot
                     then update sol c' s_false else sol
                   in
                     Solution.join sol_true sol_false
               | (_, SEQ (c1, _)) -> update sol c1 s
               | (_, END) -> sol
               | _ -> update sol c' s'
        )
        t Solution.bot

  let widen : program -> solution -> solution -> solution
    = fun pgm sol1 sol2 ->
      Solution.fold
        (fun l s sol ->
           let (_, stmt) = get_some (cmd_of_label pgm l) in
           let s' = Solution.image sol l in
           let s'' =
             (match stmt with
                  WHILE _ -> State.widen s s'
                | _ -> State.join s s'
             )
           in
             Solution.update sol l s''
        )
        sol1 sol2

  let narrow : program -> solution -> solution -> solution
    = fun pgm sol1 sol2 ->
      Solution.fold
        (fun l s sol ->
           let (_, stmt) = get_some (cmd_of_label pgm l) in
           let s' = Solution.image sol l in
           let s'' =
             (match stmt with
                  WHILE _ -> State.narrow s s'
                | _ -> s'
             )
           in
             Solution.update sol l s''
        )
        sol1 sol2

  let rec fix_with_widening :
      program -> solution -> solution
    = fun pgm sol ->
      let sol' = next pgm sol in
      let sol'' = widen pgm sol sol' in
        if Solution.leq sol'' sol
        then sol
        else fix_with_widening pgm sol''

  let rec narrowing :
      program -> solution -> solution
    = fun pgm sol ->
      let sol' = next pgm sol in
      let sol'' = narrow pgm sol sol' in
        if Solution.leq sol sol''
        then sol''
        else narrowing pgm sol''

  let analyze : program -> solution
    = fun ((l, _) as pgm) ->
      let sol0 = Solution.make [(l, State.bot)] in
      let sol_widened = fix_with_widening pgm sol0 in
        narrowing pgm sol_widened

  let get_interval : value -> interval
    = fun v -> Fact.l (Value.r v)

  let get_parity_of : string -> state -> parity
    = fun x s -> get_parity (State.image s x)

  let string_of_value : value -> string
    = fun v ->
      let itv = get_interval v in
      let prty = get_parity v in
        Interval.string_of (Zintvl.to_interval itv) ^ ", " ^
          Reminder.string_of (Rem.to_reminder prty)

  let string_of_state : state -> string
    = fun s ->
      State.fold
        (fun x f s ->
           s ^ x ^ ":" ^ string_of_value f ^ "\n")
        s ""

  let string_of_sol : solution -> string
    = fun sol ->
      Solution.fold (fun l s str ->
                       str ^ "## " ^ (string_of_int l) ^ "\n" ^ (string_of_state s) ^ "\n"
                    ) sol ""

  let get_all_davinci_vars sol =
    let s = Solution.fold (fun l s s' -> State.join s s') sol State.bot in
      State.fold (fun l v ls ->
                    let r = Fact.r (Value.r v) in
                      if Rem.is_davinci r then l :: ls
                      else ls
                 ) s []
end

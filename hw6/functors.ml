(* set functors for
 * SNU 4541.664A Program Analysis
 * Kwangkeun Yi, 2010
 *)

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
              B.fold (fun b c -> add (a,b) c)
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
end

module type FLAT_DOMAIN =
sig
  include DOMAIN
  type atom
  val make: atom -> elt
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
end

module type INTERVAL_DOMAIN =
sig
  include DOMAIN
  exception Undefined
  type bound = Z of int | Pinfty | Ninfty
  val l: elt -> bound  (* lower bound *)
  val u: elt -> bound  (* upper bound *)
  val make: bound -> bound -> elt
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
    | (x, y) -> if x=y then x else TOP
  let leq x y = match (x, y)
  with (BOT, _) -> true
    | (_, TOP) -> true
    | (ELT a, ELT b) -> a=b
    | _ -> false
  let make a = ELT a
end

module ProductDomain (A: DOMAIN) (B: DOMAIN) : PRODUCT_DOMAIN
  with type lelt = A.elt and type relt = B.elt =
struct
  type elt = BOT | TOP | ELT of A.elt * B.elt
  type lelt = A.elt
  type relt = B.elt
  let bot = BOT
  let top = TOP
  let join x y = match (x,y)
  with (BOT,_) -> y
    | (TOP,_) -> TOP
    | (_,BOT) -> x
    | (_,TOP) -> TOP
    | (ELT(a,b), ELT(a',b')) -> ELT(A.join a a', B.join b b')
  let leq x y = match (x,y)
  with (BOT,_) -> true
    | (_,TOP) -> true
    | (TOP,_) -> false
    | (_,BOT) -> false
    | (ELT(a,b), ELT(a',b')) -> (A.leq a a') && (B.leq b b')
  let l x = match x with TOP -> A.top | BOT -> A.bot | ELT(a,b) -> a
  let r x = match x with TOP -> B.top | BOT -> B.bot | ELT(a,b) -> b
  let make a b = ELT (a,b)
end

module PowersetDomain (A: SET) : POWERSET_DOMAIN with type atom = A.elt =
struct
  type elt = BOT | TOP | ELT of A.t
  type atom = A.elt
  let bot = BOT
  let top = TOP
  let join x y = match (x,y)
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
        (ELT (A.fold (fun a s -> A.add (f a) s) (A.all()) A.empty))
    | ELT s ->
        (ELT (A.fold (fun a s' -> A.add (f a) s') s A.empty))

  let make lst = match lst
  with [] -> BOT
    | l -> ELT
        (List.fold_left (fun s x -> A.add x s)
           A.empty l
        )

  (* ... *)
  let rec remove a x = match x with
      BOT -> BOT
    | TOP ->
        remove a (ELT (A.all()))
          (* ELT (A.fold (fun a' s -> if a != a' A.add a' s else s) (A.all()) A.empty) *)
    | ELT s ->
        ELT (A.filter ((!=) a) s)
          (* QnA: 만약 remove, diff나 inter의 결과가 empty set이 되더라도 BOT로 바꿀 필요 없음?
             let s' = A.filter ((!=) a) s in
             if A.is_empty s' then
             BOT
             else
             ELT s'
          *)

  let rec diff x y = match (x, y) with
      BOT, _ -> BOT
    | _, BOT -> x
    | _, TOP -> BOT
    | TOP, _ -> diff (ELT (A.all())) y
    | ELT s1, ELT s2 ->
        ELT (A.filter (fun a -> not(A.mem a s2)) s1)
          (* QnA: 만약 remove, diff나 inter의 결과가 empty set이 되더라도 BOT로 바꿀 필요 없음?
             let s' = A.filter (fun a -> not(A.mem a s2)) s1 in
             if A.is_empty s' then
             BOT
             else
             ELT s'
          *)

  let inter x y = match (x, y) with
      BOT, _ -> BOT
    | _, BOT -> BOT
    | TOP, _ -> y
    | _, TOP -> x
    | ELT s1, ELT s2 ->
        ELT (A.filter (fun a -> A.mem a s2) s1)
          (* QnA: 만약 remove, diff나 inter의 결과가 empty set이 되더라도 BOT로 바꿀 필요 없음?
             let s' = A.filter (fun a -> A.mem a s2) s1 in
             if A.is_empty s' then
             BOT
             else
             ELT s'
          *)

  let union x y = join x y

  let leq x y = match (x, y) with
      (BOT,_) -> true
    | (_,TOP) -> true
    | (TOP,_) -> false
    | (_,BOT) -> false
    | (ELT _, ELT _) -> (inter x y) == x
        (* QnA: BOT <= ..., ... <= TOP ?? *)

(* ... *)
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

  (* ... *)
  let make lst = match lst with
      [] -> BOT
    | l -> ELT (List.fold_left (fun m (l, r) -> Map.add l r m) Map.empty l)

  let tt = make (A.fold (fun a lst -> (a, B.top) :: lst) (A.all()) [])


  let bb = make (A.fold (fun a lst -> (a, B.bot) :: lst) (A.all()) [])

  let rec fold f x a = match x with
      BOT -> fold f bb a
    | TOP -> fold f tt a
    | ELT s -> Map.fold f s a

  let rec map f x = match x with
      BOT -> map f bb
    | TOP -> map f tt
    | ELT s -> make (Map.fold (fun l r lst -> (f l r) :: lst) s [])

  let rec update x l r = match x with
      BOT -> update bb l r
    | TOP -> update tt l r
    | ELT s -> ELT (Map.add l r s)

  let image x l = match x with
      BOT -> B.bot
    | TOP -> B.top
    | ELT s -> Map.find l s

  let leq x y = match (x, y) with
      BOT, _ -> true
    | _, TOP -> true
    | TOP, _ -> false
    | _, BOT -> false
    | ELT s1, ELT s2 ->
        Map.fold (fun l r b -> (B.leq r (Map.find l s2)) && b) s1 true

  let join x y = match (x, y) with
      (BOT, _) -> y
    | (_, BOT) -> x
    | (TOP, _) -> TOP
    | (_, TOP) -> TOP
    | (ELT s1, ELT s2) ->
        ELT (Map.fold (fun l r m ->
                         Map.add l (B.join r (Map.find l m)) m
                      ) s1 s2
            )
          (* ... *)
end

module Zintvl : INTERVAL_DOMAIN =
struct
  (* ... *)
  type bound = Z of int | Pinfty | Ninfty
  type elt = BOT | ELT of bound * bound
  let bot = BOT
  let top = ELT (Ninfty, Pinfty)

  exception Undefined

  let (<) i j = match (i, j) with
      Pinfty, _ -> false
    | _, Pinfty -> true
    | Ninfty, _ -> true
    | _, Ninfty -> false
    | Z i, Z i' -> i < i'

  let (>) i j = j < i

  let max i j = if i < j then j else i
  let min i j = if i < j then i else j

  let join x y = match (x, y) with
      BOT, _ -> y
    | _, BOT -> x
    | ELT(l, h), ELT(l', h') -> ELT(min l l', max h h')

  let leq x y = match (x, y) with
      BOT, _ -> true
    | _, BOT -> false
    | ELT(l, h), ELT(l', h') -> (l > l') && (h < h')

  let l x = match x with
      BOT -> raise Undefined
    | ELT(l, h) -> l

  let u x = match x with
      BOT -> raise Undefined
    | ELT(l, h) -> h

  let make i j =
    if i > j then
      raise Undefined
    else
      ELT (i, j)
end

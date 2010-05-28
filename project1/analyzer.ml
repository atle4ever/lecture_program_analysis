(* Analyzer code *)
open K

(* start Domain *)
module Parity =
struct
  include Domain.Parity

  let make : int -> t =
    fun i -> if (i mod 2) == 0 then EVEN else ODD

  let add : t -> t -> t =
    fun p1 p2 -> match p1, p2 with
        TOP, _ -> p1
      | _, TOP -> p2
      | p1, p2 -> if eq p1 p2 then even else odd
end

module Interval =
struct
  include Domain.Interval

  let top = ELT (Ninfty, Pinfty)

  let make : int -> int -> t =
    fun l h ->
      if h < l then
        raise Undefined
      else
        ELT (Z l, Z h)

  let add_bound : bound -> bound -> bound =
    fun b1 b2 -> match b1, b2 with
        Pinfty, Ninfty | Ninfty, Pinfty -> raise (Error "Invalid operation (Pinfty + Ninfty)")
      | Pinfty, _ -> b1
      | _, Pinfty -> b2
      | Ninfty, _ -> b2
      | _, Ninfty -> b1
      | Z z1, Z z2 -> Z (z1 + z2)

  let less_bound : bound -> bound -> bool =
    fun b1 b2 -> match b1, b2 with
        Pinfty, Pinfty | Ninfty, Ninfty -> false
      | _, Pinfty | Ninfty, _ -> true
      | Z i1, Z i2 -> i1 < i2
      | _ -> false

  let desc_bound : bound -> bound =
    fun b -> match b with
        Pinfty | Ninfty -> b
      | Z i -> Z (i-1)

  let incr_bound : bound -> bound =
    fun b -> match b with
        Pinfty | Ninfty -> b
      | Z i -> Z (i+1)

  let minus_bound : bound -> bound =
    fun b -> match b with
        Z n -> Z(-n)
      | Pinfty -> Ninfty
      | Ninfty -> Pinfty

  let join : t -> t -> t =
    fun itv1 itv2 -> match itv1, itv2 with
        ELT (l1, h1), ELT (l2, h2) ->
          ELT (min_bound l1 l2, max_bound h1 h2)
      | _ -> raise (Error "Invalid join of Interval")

  let widening : t -> t -> t =
    fun itv1 itv2 -> match itv1, itv2 with
        ELT (l1, h1), ELT (l2, h2) ->
          ELT ((if less_bound l2 l1 then Ninfty else l1), (if less_bound l1 h2 then Pinfty else h2))
      | _ -> raise (Error "Invalid widening of Interval")

  let narrowing : t -> t -> t =
    fun itv1 itv2 -> match itv1, itv2 with
        ELT (l1, h1), ELT (l2, h2) ->
          ELT ((if l1 == Ninfty then l2 else l1), (if h1 == Pinfty then h2 else h1))
      | _ -> raise (Error "Invalid narrowing of Interval")

  let add : t -> t -> t =
    fun i1 i2 -> match i1, i2 with
        ELT (l1, h1), ELT (l2, h2) -> ELT (add_bound l1 l2, add_bound h1 h2)
      | _ -> raise (Error "Invalid add of Interval")

  let minus : t -> t =
    fun i -> match i with
        ELT (l, h) -> ELT (minus_bound h, minus_bound l)
      | _ -> raise (Error "Invalid minus of Interval")

  let elt_of : t -> (bound * bound) =
    fun i -> match i with
        ELT (l, h) -> (l, h)
      | _ -> raise (Error "Invalid 'elt_of' of Interval")
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

  let widening (p1, i1) (p2, i2) =
    (Parity.join p1 p2, Interval.widening i1 i2)

  let narrowing (p1, i1) (p2, i2) =
    (Parity.join p1 p2, Interval.narrowing i1 i2)

  let eq (p1, i1) (p2, i2) =
    (Parity.eq p1 p2) && (Interval.eq i1 i2)

  let add (p1, i1) (p2, i2) =
    (Parity.add p1 p2, Interval.add i1 i2)

  let minus (p, i) = (p, Interval.minus i)

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

  let widening v1 v2 =
    match v1, v2 with
        Z z1, Z z2 -> Z (Z.widening z1 z2)
      | Bool b1, Bool b2 -> Bool (Bool.join b1 b2)
      | Loc l1, Loc l2 -> Loc (Loc.join l1 l2)
      | _, _ -> TOP

  let narrowing v1 v2 =
    match v1, v2 with
        Z z1, Z z2 -> Z (Z.narrowing z1 z2)
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

  let widening m1 m2 =
    fold (fun x v1 m' ->
            try let v2 = lookup x m' in bind x (Val.widening v1 v2) m'
            with Not_found -> bind x v1 m'
         ) m1 m2

  let narrowing m1 m2 =
    fold (fun x v1 m' ->
            try let v2 = lookup x m' in bind x (Val.narrowing v1 v2) m'
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

  let widening sm1 sm2 =
    fold (fun c m sm ->
            try let m2 = find c sm in add c (M.widening m m2) sm
            with Not_found -> add c m sm
         ) sm1 sm2

  let narrowing sm1 sm2 =
    fold (fun c m sm ->
            try let m2 = find c sm in add c (M.narrowing m m2) sm
            with Not_found -> add c m sm
         ) sm1 sm2

  let singleton c m =
    add c m empty

  let eq sm1 sm2 =
    equal M.eq sm1 sm2
end
(* end Domain *)


module type ANALYZER =
sig
  type state
  type solution

  val n : program -> label -> cmd
  val analyze : program -> solution
  val get_state : label -> solution -> state
  val get_parity_of : id -> state -> Parity.t
  val get_interval_of : id -> state -> Interval.t
end

module Analyzer : ANALYZER =
struct
  type state = State.t
  type solution = label -> state

(*
  Returns next command of the given label.
  All given labels are assumed to be valid.
  If there is no command left, END is returned.
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

  (****************)
  let rec eval : exp -> (Val.t M.t) -> Val.t
    = fun e m -> match e with
        NUM i -> Val.Z (Parity.make i, Interval.make i i)
      | TRUE -> Val.Bool Bool.mt
      | FALSE -> Val.Bool Bool.mf
      | ADD (e1, e2) ->
          let z1 = Val.z_of (eval e1 m) in
          let z2 = Val.z_of (eval e2 m) in
            Val.Z (Z.add z1 z2)
      | MINUS (e1) ->
          let z = Val.z_of (eval e1 m) in
            Val.Z (Z.minus z)
      | VAR x -> M.lookup x m
      | STAR x ->
          let ls = Val.l_of (M.lookup x m) in
          let l = Loc.min_elt ls in
          let v = M.lookup l m in
            Loc.fold (fun l v -> Val.join v (M.lookup l m)) ls v
      | AMPER x -> Val.Loc (Loc.singleton x)
      | READ -> Val.Z (Parity.top, Interval.top)
      | LESS (e1, e2) ->
          let _, i1 = Val.z_of (eval e1 m) in
          let _, i2 = Val.z_of (eval e2 m) in
            match i1, i2 with
                Interval.ELT (l1, h1), Interval.ELT (l2, h2) ->
                  if (* h1 < l2 *) Interval.less_bound h1 l2 then Val.Bool Bool.mt
                  else if (* h2 < l1 *) Interval.less_bound h2 l1 then Val.Bool Bool.mf
                  else Val.Bool Bool.top
              | _ -> raise (Error "Invalid beval")

  let part_left : Z.t -> Interval.t -> (Val.t * Val.t) =
    fun z i' ->
      let p, i = z in
      let l, h = Interval.elt_of i in
      let l', h' = Interval.elt_of i' in
      let h'' = Interval.desc_bound h' in
      (
        Val.Z (p, Interval.ELT (l, Interval.min_bound h'' h)),
        Val.Z (p, Interval.ELT (Interval.max_bound l' l, h))
      )

  let part_right : Z.t -> Interval.t -> (Val.t * Val.t) =
    fun z i' ->
      let p, i = z in
      let l, h = Interval.elt_of i in
      let l', h' = Interval.elt_of i' in
      let l'' = Interval.incr_bound l' in
        (
          Val.Z (p, Interval.ELT (Interval.max_bound l'' l, h)),
          Val.Z (p, Interval.ELT (l, Interval.max_bound h h'))
        )

  let part : exp -> exp -> (Val.t M.t) -> (Val.t M.t * Val.t M.t) =
    fun e1 e2 m ->
      let (p1, i1) = Val.z_of (eval e1 m) in
      let (p2, i2) = Val.z_of (eval e2 m) in
        match e1, e2 with
            VAR x, _ ->
              let (v1, v2) = part_left (p1, i1) i2 in
                (M.add x v1 m, M.add x v2 m)
          | _, VAR x ->
              let (v1, v2) = part_right (p2, i2) i1 in
                (M.add x v1 m, M.add x v2 m)
          | _ -> (m, m)

  let next : program -> State.t -> (Val.t M.t StateMap.t)
    = fun pgm s ->
      match s with
          (l, SKIP), m -> StateMap.singleton (n pgm l) m
        | (l, ASSIGN (x, e)), m ->
            let v = eval e m in
              StateMap.singleton (n pgm l) (M.bind x v m)
        | (l, ASSIGNSTAR(x, e)), m ->
            let ls = Val.l_of (M.lookup x m) in
            let v = eval e m in
            let nc = n pgm l in
              Loc.fold (
                fun l sm ->
                  StateMap.join (StateMap.singleton nc (M.bind l v m)) sm
              ) ls StateMap.empty
        | (l, SEQ (c1, c2)), m -> StateMap.singleton c1 m
        | (l, IF (e, c1, c2)), m ->
            let b = Val.b_of (eval e m) in
              (
                match b with
                    Bool.TRUE -> StateMap.singleton c1 m
                  | Bool.FALSE -> StateMap.singleton c2 m
                  | Bool.TOP ->
                      match e with
                          VAR x -> StateMap.join (StateMap.singleton c1 (M.bind x (Val.Bool Bool.mt) m)) (StateMap.singleton c2 (M.bind x (Val.Bool Bool.mf) m))
                        | STAR x -> StateMap.join (StateMap.singleton c1 m) (StateMap.singleton c2 m)
                        | LESS (e1, e2) ->
                            let m1, m2 = part e1 e2 m in
                              StateMap.join (StateMap.singleton c1 m1) (StateMap.singleton c2 m2)
                        | _ -> raise (Error "Invalid condition of if")
              )
        | (l, WHILE (e, c1)), m ->
            let b = Val.b_of (eval e m) in
              (
                match b with
                    Bool.TRUE -> StateMap.singleton c1 m
                  | Bool.FALSE -> StateMap.singleton (n pgm l) m
                  | Bool.TOP ->
                      match e with
                          VAR x -> StateMap.join (StateMap.singleton c1 (M.bind x (Val.Bool Bool.mt) m)) (StateMap.singleton (n pgm l) (M.bind x (Val.Bool Bool.mf) m))
                        | STAR x -> StateMap.join (StateMap.singleton c1 m) (StateMap.singleton (n pgm l) m)
                        | LESS (e1, e2) ->
                            let m1, m2 = part e1 e2 m in
                              StateMap.join (StateMap.singleton c1 m1) (StateMap.singleton (n pgm l) m2)
                        | _ -> raise (Error "Invalid condition of while")
              )
        | (l, END), m -> StateMap.empty

  let large_next : program -> (Val.t M.t StateMap.t) -> (Val.t M.t StateMap.t)
    = fun pgm sm ->
      StateMap.fold (fun c m sm ->
                       let sm2 = next pgm (c, m) in
                         StateMap.join sm sm2
                    ) sm StateMap.empty

  let rec widening pgm sm =
    let sm' = StateMap.widening sm (large_next pgm sm) in
      if StateMap.eq sm sm' then
        sm'
      else
        widening pgm sm'

  let rec narrowing pgm sm =
    let sm' = StateMap.narrowing sm (large_next pgm sm) in
      if StateMap.eq sm sm' then
        sm'
      else
        narrowing pgm sm'

  let rec find_cmd : label -> program -> cmd =
    fun l pgm ->
      let l', stmt = pgm in
        if l == (-1) then (l, END)
        else if l' == l then pgm
        else (
          match stmt with
            | SEQ (cmd1, cmd2) -> (try find_cmd l cmd1 with Error _ -> find_cmd l cmd2)
            | IF (be, cmd1, cmd2) -> (try find_cmd l cmd1 with Error _ -> find_cmd l cmd2)
            | WHILE (be, cmd1) -> find_cmd l cmd1
            | _ -> raise (Error ("Invalid label(" ^ (string_of_int l) ^ ")"))
        )

  let analyze : program -> solution
    = fun pgm ->
      let sm = StateMap.add pgm M.empty StateMap.empty in
      let sm' = widening pgm sm in
      let sm'' = narrowing pgm sm' in
        StateMap.iter (fun (l, stmt) m ->
                         print_string ("label " ^ (string_of_int l) ^ ":\n" ^ M.string_of m);
                         print_newline()
                      ) sm'';
        StateMap.fold (fun (l, stmt) m sol ->
                         fun l' ->
                           if l' == l then ((l, stmt), m)
                           else sol l'
                      ) sm'' (fun l' -> (find_cmd l' pgm, M.empty))

  let get_state : label -> solution -> state =
    fun l sol -> sol l

  let get_parity_of : id -> state -> Parity.t =
    fun id s ->
      let c, m = s in
        try let v = M.lookup id m in
          match v with
              Val.Z (p, _) -> p
            | Val.TOP -> Parity.top
            | _ -> Parity.bot
        with Not_found -> Parity.bot

  let get_interval_of : id -> state -> Interval.t =
    fun id s ->
      let c, m = s in
        try let v = M.lookup id m in
          match v with
              Val.Z (_, i) -> i
            | Val.TOP -> Interval.top
            | _ -> Interval.bot
        with Not_found -> Interval.bot

    (****************)
end

(* Analyzer code *)
open K
open Domain

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
  type state
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

  let analyze : program -> solution
    = fun pgm ->
      let add_p : Parity.t -> Parity.t -> Parity.t
        = fun p1 p2 ->
          match p1, p2 with
              Parity.TOP, _ -> p1
            | _, Parity.TOP -> p2
            | Parity.BOT, _ -> p2
            | _, Parity.BOT -> p1
            | p1, p2 -> if Parity.eq p1 p2 then Parity.even else Parity.odd
      in

      let add_i : Interval.t -> Interval.t -> Interval.t
        = fun i1 i2 ->
          match i1, i2 with
              Interval.TOP, _ -> i1
            | _, Interval.TOP -> i2
            | Interval.BOT, _ -> i2
            | _, Interval.BOT -> i1
            | Interval.ELT (l1, h1), Interval.ELT (l2, h2) ->
                Interval.ELT (Interval.min_bound l1 l2, Interval.max_bound h1 h2)
      in
      let add : Z.t -> Z.t -> Z.t
        = fun (p1, i1) (p2, i2) -> (add_p p1 p2, add_i i1 i2)
      in
      let minus_b : Interval.bound -> Interval.bound
        = fun b ->
          match b with
              Interval.Z n -> Interval.Z(-n)
            | Interval.Pinfty -> Interval.Ninfty
            | Interval.Ninfty -> Interval.Pinfty
      in
      let minus_i : Interval.t -> Interval.t
        = fun i ->
          match i with
              Interval.TOP -> Interval.TOP
            | Interval.BOT -> Interval.BOT
            | Interval.ELT (l, h) -> Interval.ELT (minus_b h, minus_b l)
      in
      let minus : Z.t -> Z.t
        = fun (p, i) -> (p, minus_i i)
      in
      let p_of : int -> Parity.t
        = fun i -> if (i mod 2) == 0 then Parity.even else Parity.odd
      in
      let rec eval : exp -> (Val.t M.t) -> Val.t
        = fun e m ->
          match e with
              NUM i -> Val.Z (p_of i, Interval.make i i)
            | TRUE -> Val.Bool Bool.mt
            | FALSE -> Val.Bool Bool.mf
            | ADD (e1, e2) ->
                let z1 = Val.z_of (eval e1 m) in
                let z2 = Val.z_of (eval e2 m) in
                  Val.Z (add z1 z2)
            | MINUS (e1) ->
                let z = Val.z_of (eval e1 m) in
                  Val.Z (minus z)

            | VAR x -> M.lookup x m
            | STAR x ->
                let l = Val.l_of (M.lookup x m) in
                  Loc.fold (fun l v -> Val.join v (M.lookup l m)) l Val.BOT
            | AMPER x -> Val.Loc (Loc.singleton x)
            | READ -> Val.Z (Parity.top, Interval.top)
      in
      let beval : bexp -> (Val.t M.t) -> Bool.t
        = fun (LESS (e1, e2)) m ->
          let (_, i1) = Val.z_of (eval e1 m) in
          let (_, i2) = Val.z_of (eval e2 m) in
            match (i1, i2) with
                Interval.BOT, _ | _, Interval.BOT | Interval.TOP , _ | _, Interval.TOP -> raise (Error "Invalid Z")
              | Interval.ELT (l1, h1), Interval.ELT (l2, h2) ->
                  if Interval.eq_bound (Interval.max_bound h1 l2) l2 then Bool.mt
                  else if Interval.eq_bound (Interval.max_bound h2 l1) l1 then Bool.mf
                  else Bool.top
      in

      let next : State.t -> (Val.t M.t StateMap.t)
        = fun s ->
          match s with
              (l, SKIP), m -> StateMap.singleton (n pgm l) m
            | (l, ASSIGN (x, e)), m ->
                let v = eval e m in
                  StateMap.singleton (n pgm l) (M.bind x v m)
            | (l, SEQ (c1, c2)), m -> StateMap.singleton c1 m
            | _ -> raise (Error "Not implemented next")
      in
      let large_next : (Val.t M.t StateMap.t) -> (Val.t M.t StateMap.t)
        = fun sl ->
          StateMap.fold (fun c m sm ->
                           let sm2 = next (c, m) in
                             StateMap.join sm sm2
                        )  sl StateMap.empty
      in
        (fun x -> (pgm, M.empty))

      let get_state _ _ = raise (Error "Not implemented")
  let get_parity_of _ _ = raise (Error "Not implemented")
  let get_interval_of _ _ = raise (Error "Not implemented")
    (****************)
end

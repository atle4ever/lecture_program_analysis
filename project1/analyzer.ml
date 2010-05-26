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
  let add_p : Parity.t -> Parity.t -> Parity.t
    = fun p1 p2 ->
      match p1, p2 with
          Parity.TOP, _ -> p1
        | _, Parity.TOP -> p2
        | p1, p2 -> if Parity.eq p1 p2 then Parity.even else Parity.odd

  let add_i : Interval.t -> Interval.t -> Interval.t
    = fun (l1, h1) (l2, h2) -> (Interval.add_bound l1 l2, Interval.add_bound h1 h2)

  let add : Z.t -> Z.t -> Z.t
    = fun (p1, i1) (p2, i2) -> (add_p p1 p2, add_i i1 i2)

  let minus_b : Interval.bound -> Interval.bound
    = fun b ->
      match b with
          Interval.Z n -> Interval.Z(-n)
        | Interval.Pinfty -> Interval.Ninfty
        | Interval.Ninfty -> Interval.Pinfty

  let minus_i : Interval.t -> Interval.t
    = fun i ->
      match i with
        (l, h) -> (minus_b h, minus_b l)

  let minus : Z.t -> Z.t
    = fun (p, i) -> (p, minus_i i)

  let rec eval : exp -> (Val.t M.t) -> Val.t
    = fun e m ->
      match e with
          NUM i -> Val.Z (Parity.convert i, Interval.make i i)
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
            let ls = Val.l_of (M.lookup x m) in
            let l = Loc.min_elt ls in
            let v = M.lookup l m in
              Loc.fold (fun l v -> Val.join v (M.lookup l m)) ls v
        | AMPER x -> Val.Loc (Loc.singleton x)
        | READ -> Val.Z (Parity.top, Interval.top)

  let beval : bexp -> (Val.t M.t) -> Bool.t
    = fun (LESS (e1, e2)) m ->
      let (_, i1) = Val.z_of (eval e1 m) in
      let (_, i2) = Val.z_of (eval e2 m) in
        match i1, i2 with
            (l1, h1), (l2, h2) ->
              if (* h1 < l2 *) Interval.less_bound h1 l2 then Bool.mt
              else if (* h2 < l1 *) Interval.less_bound h2 l1 then Bool.mf
              else Bool.top

  let part (LESS (e1, e2)) m =
    let part_left (p, (l, h)) (l', h') =
      let b = Interval.desc h' in
        (Val.Z (p, (l, b)), Val.Z (p, (l', h)))
    in

    let part_right (p, (l, h)) (l', h') =
      let b = Interval.incr l' in
        (Val.Z (p, (b, h)), Val.Z (p, (l, l')))
    in

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
              Loc.fold (fun l sm -> StateMap.join (StateMap.singleton nc (M.bind l v m)) sm) ls StateMap.empty
        | (l, SEQ (c1, c2)), m -> StateMap.singleton c1 m
        | (l, IF (be, c1, c2)), m ->
            let b = beval be m in
              (
                match b with
                    Bool.TRUE -> StateMap.singleton c1 m
                  | Bool.FALSE -> StateMap.singleton c2 m
                  | Bool.TOP ->
                      let m1, m2 = part be m in
                        StateMap.join (StateMap.singleton c1 m1) (StateMap.singleton c2 m2)
              )
        | (l, WHILE (be, c1)), m ->
            let b = beval be m in
              (
                match b with
                    Bool.TRUE -> StateMap.singleton c1 m
                  | Bool.FALSE -> StateMap.singleton (n pgm l) m
                  | Bool.TOP ->
                      let m1, m2 = part be m in
                        StateMap.join (StateMap.singleton c1 m1) (StateMap.singleton (n pgm l) m2)
              )

        | (l, END), m -> StateMap.empty

  let large_next : program -> (Val.t M.t StateMap.t) -> (Val.t M.t StateMap.t)
    = fun pgm sm ->
      StateMap.fold (fun c m sm ->
                       let sm2 = next pgm (c, m) in
                         StateMap.join sm sm2
                    )  sm StateMap.empty

  let analyze : program -> solution
    = fun pgm ->
      let rec fix sm n =
          let sm' = StateMap.join sm (large_next pgm sm) in
            if StateMap.eq sm sm' then
              (sm', true)
            else
              if n == 0 then
                (sm', false)
              else
                fix sm' (n-1)
      in
      let rec widen sm =
          let sm' = StateMap.widen sm (large_next pgm sm) in
            if StateMap.eq sm sm' then
              sm'
            else
              widen sm'
      in

      let sm = StateMap.add pgm M.empty StateMap.empty in
      let (sm', isFixed) = fix sm 10000 in
      let sm' = if isFixed then sm' else (print_string "widen"; print_newline(); widen sm') in
        StateMap.iter (fun (l, stmt) m ->
                         print_string ("label " ^ (string_of_int l) ^ ":\n" ^ M.string_of m);
                         print_newline()
                      ) sm';
        StateMap.fold (fun (l, stmt) m sol ->
                         fun l' ->
                           if l' == l then ((l, stmt), m)
                           else sol l'
                      ) sm' (fun l' -> raise (Error "Invalid Label"))


  let get_state l sol = sol l

  let get_parity_of id (c, m) =
    let v = M.lookup id m in
      match v with
          Val.Z (p, _) -> p
        | Val.TOP -> Parity.top
        | _ ->

    let (p, i) = Val.z_of (M.lookup id m) in
      p

  let get_interval_of id (c, m) =
    let (p, i) = Val.z_of (M.lookup id m) in
      i

    (****************)
end

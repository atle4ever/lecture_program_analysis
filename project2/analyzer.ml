(* Analyzer code *)
open K
open Domain
open Functors

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

module Analyzer (Loc: ID_SET) (Label: INT_SET) : ANALYZER =
struct
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
                   let sol' =
                     if s_true <> State.bot
                     then update sol c1 s_true else sol
                   in
                     if s_false <> State.bot
                     then update sol' c2 s_false else sol'
               | (_, WHILE (e, c'')) ->
                   let (s_true, s_false) = Inter.prune e s' in
                   let sol' =
                     if s_true <> State.bot
                     then update sol c'' s_true else sol
                   in
                     if s_false <> State.bot
                     then update sol' c' s_false else sol'
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
      program -> solution -> solution -> solution
    = fun pgm sol0 sol ->
      let sol' = Solution.join sol0 (next pgm sol) in
      let sol'' = widen pgm sol sol' in
        if Solution.leq sol'' sol
        then sol
        else fix_with_widening pgm sol0 sol''

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
      let sol_widened = fix_with_widening pgm sol0 sol0 in
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
    let _ = print_endline (string_of_sol sol) in
    let s = Solution.fold (fun l s s' -> State.join s s') sol State.bot in
      Loc.fold (fun v vs ->
                  let r  = Fact.r (Value.r (State.image s v)) in
                  if Rem.is_davinci r then v :: vs
                  else vs
               ) (Loc.all()) []
end

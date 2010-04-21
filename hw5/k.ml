exception Error of string
type id = string
type label = int
type cmd = label * stmt
and stmt = SKIP
       | ASSIGN of id * exp
       | ASSIGNSTAR of id * exp
       | SEQ of cmd * cmd
       | IF of exp * cmd * cmd
       | WHILE of exp * cmd
and exp = NUM of int
      | ADD of exp * exp
      | MINUS of exp
      | VAR of id
      | STAR of id
      | AMPER of id
      | READ
type program = cmd

module OrderedTypeForMemory : (Set.OrderedType with type t = Memory.t) =
struct
  type t = Memory.t
  let compare m1 m2 = if (Memory.equal m1 m2) then 0
  else compare (Memory.string_of_memory m1) (Memory.string_of_memory m2)
end

module MemorySet = Set.Make(OrderedTypeForMemory)

type memory = Memory.t
type trace = memory list
type traces = trace list
type states = MemorySet.t
type pointstates = label -> MemorySet.t
let emptyMemory = Memory.empty

(***)

module OrderedTypeForValue : (Set.OrderedType with type t = Value.t) =
struct
  type t = Value.t
  let compare v1 v2 = match (v1, v2) with
      Value.INT i1, Value.INT i2 -> compare i1 i2
    | Value.LOC s1, Value.LOC s2 -> compare s1 s2
    | _, _ -> -1
end
module VS = Set.Make(OrderedTypeForValue)

let emptyMS = MemorySet.empty
let emptyVS = VS.empty

let (@+) ms1 ms2 = MemorySet.union ms1 ms2
let (@<<) ms m = MemorySet.add m ms
let ($+) vs1 vs2 = VS.union vs1 vs2
let ($<<) vs v = VS.add v vs

let rec eval_exp e ms = match e with
    NUM i -> emptyVS $<< (Value.INT i)
  | ADD (e1, e2) ->
      let addVi vi1 vi2 =
        match(vi1, vi2) with
            Value.INT i1, Value.INT i2 -> Value.INT (i1 + i2)
          | _, _ -> raise (Error "LOC can be added")
      in
      let vs1 = eval_exp e1 ms in
      let vs2 = eval_exp e2 ms in
        VS.fold (fun vi1 vs ->
                   vs $+ (VS.fold (fun vi2 vs ->
                              vs $<< (addVi vi1 vi2)
                           ) vs1 emptyVS)
                ) vs2 emptyVS

  | MINUS (e1) ->
      let vs1 = eval_exp e1 ms in
        VS.fold (fun (Value.INT i) vs -> VS.add (Value.INT (-1*i)) vs) vs1 emptyVS

  | VAR x -> MemorySet.fold (fun m vs -> vs $<< (Memory.lookup x m)) ms emptyVS
  | STAR x ->
      let vs = eval_exp (VAR x) ms in
        VS.fold (fun (Value.LOC s) vs -> VS.union (eval_exp (VAR s) ms) vs) vs emptyVS

  | AMPER x -> VS.add (Value.LOC x) emptyVS
  | READ ->
      List.fold_left (fun vs i -> VS.add (Value.INT i) vs) emptyVS [-5; -4; -3; -2; -1; 0; 1; 2; 3; 4; 5]

let tracingEval c m = []
let collectingEval (l, s) m =
  let initialMS = emptyMS @<< emptyMemory in
  let rec eval s cm = match s with
      SKIP -> (cm, cm)
    | ASSIGN(x, e) ->
        let vs = eval_exp e cm in
        let new_cm = MemorySet.fold (fun m ms ->
                                       ms @+ (VS.fold (fun v ms ->
                                                         ms @<< (Memory.bind x v m)
                                                      ) vs emptyMS)
                                    ) cm emptyMS
        in
          (new_cm, cm @+ new_cm)
    | ASSIGNSTAR(x, e) ->
        let vs = eval_exp e cm in
        let vx = eval_exp (VAR x) cm in
        let new_cm = VS.fold (fun v ms ->
                                ms @+ (VS.fold (fun (Value.LOC x) ms ->
                                           ms @+ (MemorySet.fold (fun m ms ->
                                                             ms @<< (Memory.bind x v m)
                                                          ) ms emptyMS)
                                        ) vx emptyMS)
                             ) vs emptyMS
        in
          (new_cm, cm @+ new_cm)

    | SEQ((l1, s1), (l2, s2)) ->
        let new_cm = fst(eval s1 cm) in
        let new_cm' = fst(eval s2 new_cm) in
          (new_cm', cm @+ new_cm @+ new_cm')

    | IF(e, (l1, s1), (l2, s2)) ->
        let vs = eval_exp e cm in
        let new_cm = VS.fold (fun v ms ->
                                ms @+ (if v == (Value.INT 0) then
                                         fst(eval s1 cm)
                                       else
                                         fst(eval s2 cm))
                             ) vs emptyMS
        in
          (new_cm, cm @+ new_cm)

    | WHILE(e, (l1, s1)) ->
        let vs = eval_exp e cm in
        let new_cm = VS.fold (fun v ms ->
                                ms @+ (if v == (Value.INT 0) then
                                         cm
                                       else
                                         fst(eval s (fst (eval s1 cm))))
                             ) vs emptyMS
        in
          (new_cm, cm @+ new_cm)
  in

  let (cm, tm) = eval s initialMS in
    tm

let pointCollectingEval c m =
  (fun x -> emptyMS)

(****)
let rec string_of_exp e = match e with
    NUM i -> string_of_int i
  | ADD (e1, e2) -> "("^(string_of_exp e1)^" + "^(string_of_exp e2)^")"
  | MINUS (e1) -> "-("^(string_of_exp e1)^")"
  | VAR x -> x
  | STAR x -> "* "^x
  | AMPER x -> "& "^x
  | READ -> "read"
and string_of_stmt s = match s with
    SKIP -> "skip"
  | ASSIGN(x, e) -> x^" := "^(string_of_exp e)
  | ASSIGNSTAR(x, e) -> "*"^x^" := "^(string_of_exp e)
  | SEQ(c1, c2) -> (string_of_cmd c1)^";\n"^(string_of_cmd c2)
  | IF(e, c1, c2) -> "if "^(string_of_cmd c1)^"\n"^(string_of_cmd c2)
  | WHILE(e, c) -> "while "^(string_of_exp e)^"\n"^(string_of_cmd c)
and string_of_cmd (l, stmt) =
  (string_of_int l)^": "^(string_of_stmt stmt)

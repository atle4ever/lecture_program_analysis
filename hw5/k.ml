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

(* Set of Value *)
module OrderedTypeForValue : (Set.OrderedType with type t = Value.t) =
struct
  type t = Value.t
  let compare v1 v2 = match (v1, v2) with
      Value.INT i1, Value.INT i2 -> compare i1 i2
    | Value.LOC s1, Value.LOC s2 -> compare s1 s2
    | _, _ -> -1
end
module VS = Set.Make(OrderedTypeForValue)

(* Map from label to MemorySet *)
module OrderedTypeForInt : (Map.OrderedType with type t = label) =
struct
  type t = label
  let compare = compare
end
module LMM = Map.Make(OrderedTypeForInt)

let emptyMS = MemorySet.empty
let emptyVS = VS.empty
let emptyLMM = LMM.empty

let (@@) ms1 ms2 = MemorySet.union ms1 ms2
let (@<<) ms m = MemorySet.add m ms
let ($@) vs1 vs2 = VS.union vs1 vs2
let ($<<) vs v = VS.add v vs
let (%<<) lmm (l, m) =
  let ms = LMM.find l lmm in
    LMM.add l (ms @<< m) lmm

let int_of_value v = match v with
    Value.INT i -> i
  | Value.LOC _ -> raise (Error "LOC is used as INT type")

let loc_of_value v = match v with
    Value.LOC l -> l
  | Value.INT _ -> raise (Error "INT is used as LOC type")

let rec eval_exp e m = match e with
    NUM i -> emptyVS $<< (Value.INT i)
  | ADD (e1, e2) ->
      let ($+) v1 v2 = Value.INT ((int_of_value v1) + (int_of_value v2)) in
      let vs1 = eval_exp e1 m in
      let vs2 = eval_exp e2 m in
        VS.fold (fun v1 vs ->
                   vs $@ (VS.fold (fun v2 vs ->
                                     vs $<< (v1 $+ v2)
                                  ) vs1 emptyVS)
                ) vs2 emptyVS

  | MINUS (e1) ->
      let vs1 = eval_exp e1 m in
        VS.fold (fun v vs -> vs $<< (Value.INT (-1*(int_of_value v)))) vs1 emptyVS

  | VAR x ->
      (try
        emptyVS $<< (Memory.lookup x m)
      with Not_found ->
        raise (Error ("var "^x^" is not binded")))

  | STAR x ->
      let vs = eval_exp (VAR x) m in
        VS.fold (fun v vs -> vs $@ (eval_exp (VAR (loc_of_value v)) m)) vs emptyVS

  | AMPER x -> emptyVS $<< (Value.LOC x)

  | READ ->
      List.fold_left (fun vs i -> vs $<< (Value.INT i)) emptyVS [-5; -4; -3; -2; -1; 0; 1; 2; 3; 4; 5]

let tracingEval c m =
  let rec eval (l, s) t =
    let m = List.hd t in
      match s with
          SKIP -> [m :: t]

        | ASSIGN(x, e) ->
            let vs = eval_exp e m in
              VS.fold (fun v ts -> ((Memory.bind x v m) :: t) :: ts) vs []

        | ASSIGNSTAR(x, e) ->
            let vs = eval_exp e m in
            let xvs = eval_exp (VAR x) m in
            VS.fold (fun v ts ->
                       ts @ (VS.fold (fun xv ts ->
                                        ((Memory.bind (loc_of_value xv) v m) :: t) :: ts
                                     ) xvs []
                            )
                    ) vs []

        | SEQ(c1, c2) ->
            let ts = eval c1 t in
              List.fold_left (fun ts t -> (eval c2 t) @ ts) [] ts

        | IF(e, c1, c2) ->
            let vs = eval_exp e m in
              VS.fold (fun v ts -> ts @ (if (int_of_value v) != 0 then eval c1 t else eval c2 t)) vs []

        | WHILE(e, c1) ->
            let vs = eval_exp e m in
              VS.fold (fun v ts -> ts @ (if (int_of_value v) == 0 then
                                           [t]
                                         else
                                           eval (l, SEQ(c1, (l, s))) t
                                        )) vs []
  in
    List.map List.rev (eval c [m])

let collectingEval c m =
  let ts = tracingEval c m in
    List.fold_left (fun ms t ->
                      ms @@ (List.fold_left (@<<) emptyMS t)
                   ) emptyMS ts

let pointCollectingEval c m =
  let rec collectLabel (l, s) =
    match (l, s) with
        l, SKIP | l, ASSIGN _ | l, ASSIGNSTAR _ -> [l]
      | l, SEQ(c1, c2) | l, IF(_, c1, c2) ->
          l :: (collectLabel c1) @ (collectLabel c2)
      | l, WHILE(_, c1) ->
          l :: (collectLabel c1)
  in
  let label_of_cmd (l, s) = l in
  let rec eval (l, s) m ll =
    let ll = ll %<< (l, m) in
      match s with
          SKIP -> (emptyMS @<< m, ll)

        | ASSIGN(x, e) ->
            let vs = eval_exp e m in
              (VS.fold (fun v ms -> ms @<< (Memory.bind x v m)) vs emptyMS, ll)

        | ASSIGNSTAR(x, e) ->
            let vs = eval_exp e m in
            let xvs = eval_exp (VAR x) m in
              (VS.fold (fun v ms ->
                         ms @@ (VS.fold (fun xv ms ->
                                           ms @<< (Memory.bind (loc_of_value xv) v m)
                                        ) xvs emptyMS)
                      ) vs emptyMS,
               ll)

        | SEQ(c1, c2) ->
            let (ms, ll) = eval c1 m ll in
              MemorySet.fold (fun m (ms, ll) ->
                                let (ms', ll') = eval c2 m ll in
                                  (ms @@ ms', ll')
                             ) ms (emptyMS, ll)

        | IF(e, c1, c2) ->
            let vs = eval_exp e m in
              VS.fold (fun v (ms, ll) ->
                         let (ms', ll') = (if (int_of_value v) != 0 then eval c1 m ll else eval c2 m ll) in
                           (ms @@ ms', ll')
                      ) vs (emptyMS, ll)

        | WHILE(e, c1) ->
            let vs = eval_exp e m in
              VS.fold (fun v (ms, ll) ->
                         let (ms', ll') = (if (int_of_value v) == 0 then
                                             (emptyMS @<< m, ll)
                                           else
                                             eval (label_of_cmd c1, SEQ(c1, (l, s))) m ll
                                          )
                         in
                           (ms @@ ms', ll')
                      ) vs (emptyMS, ll)
  in
  let ls = collectLabel c in
  let lmm = List.fold_left (fun lmm l -> LMM.add l emptyMS lmm) emptyLMM ls in
    LMM.fold (fun l ms f -> (fun l' -> if l' == l then ms else (f l')))
      (snd (eval c emptyMemory lmm))
      (fun l -> print_int l; raise (Error "Wrong label"))


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

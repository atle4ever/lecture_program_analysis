open Functors

module Var = PrimitiveSet(struct type t = string let compare = compare exception TooMany let all = fun () -> ["a";"b";"c";"d"] end)

module Int = PrimitiveSet(struct type t = int let compare = compare exception TooMany let all = fun () -> [0;1;2;3;4;5;6;7;8;9] end)

module VIPair = ProductSet (Var) (Int)

module VPower = PowerSet (Var)

module VIPairPower = PowerSet (VIPair)

module VtoIFun = FunctionSet (Var) (Int)

module VtoVPowerFun = FunctionSet (Var) (VIPairPower)

module A = FlatDomain (VtoVPowerFun)

module Z = PowersetDomain (Int)

module Loc = PowersetDomain (Var)

module Value = ProductDomain (Z) (Loc)

module Memory = FunDomain (Var) (Value)

module State = ProductDomain (Memory) (A)

let v = Value.make (Z.make [1;2]) (Loc.make ["a"])
let m = Memory.update Memory.bot "a" v
let v' = Memory.image m "a"
let _ = Value.leq v' v && Value.leq v v'
let (z,z') = (Value.l v, Value.l v')
let (l,l') = (Value.r v, Value.r v')
let _ = Z.leq z z'
let _ = Loc.leq l l'


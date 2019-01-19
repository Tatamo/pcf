open System
open SampleExpressions
open Reduction
printfn "Hello World from F#!"

type Type = 
  | Num
  | Bool
  | Func of Type * Type

let numType = Num
let FuncType = Func (Num, Func(Num, Num))

(*
let m = App(Var("x1"),Var("x2"))
let n1 = App(Var("x2"), Var("y"))

let s1 = substitute m "x1" n1
let s2 = substitute s1 "x2" (Var("N2"))
printfn "%A" s1
printfn "%A" s2

printfn "%A" (substitute (Lambda("y",  App(Var ("x"), Var ("y")))) "x" (Lambda("z", Var("z"))))
printfn "%A" (substitute (Lambda("y",  App(Var ("x"), Var ("y")))) "x" (Lambda("z", App(Var("z"), Var("y")))))
printfn "%A" (substitute (Fix("f",  App(Var ("f"), Var ("x")))) "x" (Lambda("z", App(Var("z"), Var("f")))))
*)

let test exp = printfn "%A" (reduceAll exp)

test applyExp
test curryFuncExp
test funcFuncExp
test simpleFixExp
test fixExp
test fixExp'

open SampleExpressions
open Reduction

let test exp = printfn "%A" (reduceAll exp)
test applyExp
test curryFuncExp
test funcFuncExp
test simpleFixExp
test fixExp
test fixExp'

open SampleExpressions
open Reduction
open Format

let test exp = printfn "%s\n=> %s" (format exp) (format (reduceAll exp))
test applyExp
test curryFuncExp
test funcFuncExp
test simpleFixExp
test fixExp
test fixExp'

module Format
open System
open Type
open Exp

let rec formatType t =
  match t with
  | Num -> "num"
  | Bool -> "bool"
  | TypeError -> "TypeError"
  | Func(t1, t2) ->
    match t1 with
    | Func(_) -> String.Format("({0}) -> {1}", formatType t1, formatType t2)
    | _ -> String.Format("{0} -> {1}", formatType t1, formatType t2)

let rec formatNumber exp =
  match exp with
  | Zero -> Some 0
  | Succ(n) -> 
    match (formatNumber n) with
    | Some(n') -> Some (n'+1)
    | _ -> None
  | _ -> None

let rec format exp =
  match exp with
    | NumericValue(e) -> 
      match formatNumber(e) with
        | Some(n) -> n.ToString()
        | _ -> "error" // unreachable
    | True -> "true"
    | False -> "false"
    | Var(s) -> s
    | Succ(e) -> String.Format ("succ {0}", format e)
    | Pred(e) -> String.Format ("pred {0}", format e)
    | IsZero(e) -> String.Format ("iszero {0}", format e)
    | If(e1, e2, e3) -> String.Format ("if {0} then {1} else {2}", format e1, format e2, format e3)
    | App(e1, e2) -> String.Format("{0} {1}", format e1, format e2)
    | Lambda((name, t), e) -> String.Format ("λ{0}:{1}.{2}", name, formatType t, format e)
    | Fix((name, t), e) -> String.Format ("μ{0}:{1}.{2}", name, formatType t, format e)
    | Error(e) ->
      match e with
      | Some(s) -> String.Format ("error({0})", s)
      | _ -> "error"
    | _ -> "error" // unreachable

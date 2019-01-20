module Reduction
open System
open Exp

// Exp中に出現する自由変数名の集合を得る
let getFreeVariables = 
  let rec fv exp =
    match exp with
    | Var(s) -> set [s]
    | App(e1, e2) -> Set.union (fv e1) (fv e2)
    | Lambda(name, _, exp) | Fix(name, _, exp) -> Set.remove name (fv exp)
    | True | False | Zero | Error(_)-> Set.empty
    | Succ(e) | Pred(e) | IsZero(e) -> fv e
    | If(e1,e2,e3) -> Set.union (fv e1) (fv e2) |> Set.union (fv e3)
  in fv

let rec getNewVariableName fv baseName =
  if Set.contains baseName fv then getNewVariableName fv (String.Format("{0}'", baseName)) else baseName

// TODO: 型チェック
let rec substitute exp1 name exp2 =
//  printfn "substitute %A[%A:=%A]" exp1 name exp2
  let mapLambda _ name exp2 (name', t, e) =
    if name' = name then (name', t, e) else
    let fv1 = getFreeVariables e in
    let fv2 = getFreeVariables exp2 in
    if Set.contains name fv1 && Set.contains name' fv2 then
      let freshName = getNewVariableName (Set.union fv1 fv2) name' in
      (freshName, t, substitute (substitute e name' (Var freshName)) name exp2)
    else
      (name', t, substitute e name exp2)
    in
  match exp1 with
  | True | False | Zero | Error(_)-> exp1
  | Succ(e) -> Succ(substitute e name exp2)
  | Pred(e) -> Pred(substitute e name exp2)
  | IsZero(e) -> IsZero(substitute e name exp2)
  | If(e1,e2,e3) -> If(substitute e1 name exp2, substitute e2 name exp2, substitute e3 name exp2)
  | Var(name') -> if name' = name then exp2 else Var(name')
  | App(e1,e2) -> App(substitute e1 name exp2, substitute e2 name exp2)
  | Lambda(name', t ,e) -> Lambda (mapLambda exp1 name exp2 (name', t, e))
  | Fix(name', t, e) -> Fix (mapLambda exp1 name exp2 (name', t, e))


let rec reduce exp =
  match exp with
  | Pred(e) -> reducePred e
  | Succ(e) -> reduceSucc e
  | IsZero(e) -> reduceIsZero e
  | If(e1,e2,e3) -> reduceIf e1 e2 e3
  | App(e1, e2) -> reduceApp e1 e2
  | Fix(name, t, e) -> reduceFix name t e
  | _ -> Error(None)
// Pred(exp) のexpが渡される
and reducePred exp =
  match exp with
  | Zero -> Zero
  | Succ(e) ->
    match e with
    | NumericValue(n) -> n
    | _ -> Pred(reduce (Succ(e)))
  | _ -> Pred(reduce exp)
and reduceSucc exp =
  match exp with
  | NumericValue(_) -> Error(Some("value cannot be reduced"))
  | _ -> Succ(reduce exp)
and reduceIsZero exp =
  match exp with
  | Zero -> True
  | Succ(e) ->
    match e with
    | NumericValue(_) -> False
    | _ -> IsZero(Succ(reduce e))
  | _ -> IsZero(reduce exp)
and reduceIf exp1 exp2 exp3 =
  match exp1 with
  | True -> exp2
  | False -> exp3
  | _ -> If(reduce exp1, exp2, exp3)
and reduceApp exp1 exp2 =
  // TODO: 型チェック
  match exp1 with
  | Lambda(name, t, exp) -> substitute exp name exp2
  | _ -> App(reduce exp1, exp2)
and reduceFix name t exp =
  substitute exp name (Fix(name,t,exp))

let rec reduceAll exp =
  if isValue exp then exp else reduceAll (reduce exp)

module Reduction
open System
open Exp

// Exp中に出現する自由変数名の集合を得る
let getFreeVariables = 
  let rec fv exp =
    match exp with
    | Var(s) -> set [s]
    | App(e1, e2) -> Set.union (fv e1) (fv e2)
    | Lambda((name, _), exp) | Fix((name, _), exp) -> Set.remove name (fv exp)
    | True | False | Zero | Error(_)-> Set.empty
    | Succ(e) | Pred(e) | IsZero(e) -> fv e
    | If(e1,e2,e3) -> Set.union (fv e1) (fv e2) |> Set.union (fv e3)
  in fv

let rec getNewVariableName fv baseName =
  if Set.contains baseName fv then getNewVariableName fv (String.Format("{0}'", baseName)) else baseName

// TODO: 型チェック
let rec substitute exp1 v exp2 =
//  printfn "substitute %A[%A:=%A]" exp1 name exp2
  let mapLambda _ v exp2 (v', e) =
    if fst v' = fst v then (v', e) else
    let fv1 = getFreeVariables e in
    let fv2 = getFreeVariables exp2 in
    if Set.contains (fst v) fv1 && Set.contains (fst v') fv2 then
      let freshName = getNewVariableName (Set.union fv1 fv2) (fst v') in
      ((freshName, (snd v')), substitute (substitute e v' (Var freshName)) v exp2)
    else
      (v', substitute e v exp2)
    in
  match exp1 with
  | True | False | Zero | Error(_)-> exp1
  | Succ(e) -> Succ(substitute e v exp2)
  | Pred(e) -> Pred(substitute e v exp2)
  | IsZero(e) -> IsZero(substitute e v exp2)
  | If(e1,e2,e3) -> If(substitute e1 v exp2, substitute e2 v exp2, substitute e3 v exp2)
  | Var(name') -> if name' = fst v then exp2 else Var(name')
  | App(e1,e2) -> App(substitute e1 v exp2, substitute e2 v exp2)
  | Lambda(v' ,e) -> Lambda (mapLambda exp1 v exp2 (v', e))
  | Fix(v', e) -> Fix (mapLambda exp1 v exp2 (v', e))


let rec reduce exp =
  match exp with
  | Pred(e) -> reducePred e
  | Succ(e) -> reduceSucc e
  | IsZero(e) -> reduceIsZero e
  | If(e1,e2,e3) -> reduceIf e1 e2 e3
  | App(e1, e2) -> reduceApp e1 e2
  | Fix(v, e) -> reduceFix v e
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
  | Lambda(v, exp) -> substitute exp v exp2
  | _ -> App(reduce exp1, exp2)
and reduceFix v exp =
  substitute exp v (Fix(v,exp))

let rec reduceAll exp =
  if isValue exp then exp else reduceAll (reduce exp)

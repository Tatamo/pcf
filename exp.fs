module Exp
open Type

type Exp = 
  | True
  | False
  | Zero
  | Var of string
  | Succ of Exp
  | Pred of Exp
  | IsZero of Exp
  | If of Exp * Exp * Exp
  | App of Exp * Exp
  | Lambda of string * Type * Exp
  | Fix of string * Type * Exp
  | Error of string option

// Succ^n(Zero) の形かどうかを判定する
let rec isNumericValue exp =
  match exp with
  | Zero -> true
  | Succ(e) -> isNumericValue(e)
  | _ -> false

let (|NumericValue|_|) (exp:Exp) =
  if isNumericValue(exp) then Some(exp) else None

let (|BooleanValue|_|) (exp:Exp) =
  match exp with
  | True -> Some(True)
  | False -> Some(False)
  | _ -> None

// それ以上簡約できない閉じた項(=値)であるかどうかを判定する
let isValue exp =
  match exp with
  | BooleanValue(_) | NumericValue(_)
  | Lambda(_) // λ抽象は常に値
  | Error(_) -> true // エラーはとりあえず値ということにしておく
  | _ -> false

let (|Value|_|) (exp:Exp) = if isValue exp then Some(exp) else None

open System
printfn "Hello World from F#!"

type Type = 
  | Num
  | Bool
  | Func of Type * Type

let numType = Num
let FuncType = Func (Num, Func(Num, Num))

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
  | Lambda of string * Exp
  | Fix of string * Exp
  | Error of string option
  // UNDONE
  // | Fix of string * Type * Exp

type Env = string -> Exp
let emptyEnv (s: string) = 
  if s = "debug" then Error(None) else Error(Some(String.Format ("Error: {0} does not exist", s)))

let update env name exp : Env =
  fun s ->
    let debug = env("debug") in
    if s = "debug" then
      match env("debug") with
      | Error(Some(e)) -> Error(Some(e+","+name+":"+exp.ToString()))
      | _ -> Error(Some(name+":"+exp.ToString()))
    else
    if s = name then exp else env(s);

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
  | Lambda(_) | Fix(_) // λ抽象は常に値
  | Error(_) -> true // エラーはとりあえず値ということにしておく
  | _ -> false

let (|Value|_|) (exp:Exp) = if isValue exp then Some(exp) else None

// Exp中に出現する自由変数名の集合を得る
let rec fv exp =
  match exp with
  | Var(s) -> set [s]
  | App(e1, e2) -> Set.union (fv e1) (fv e2)
  | Lambda(name, exp) | Fix(name, exp) -> Set.remove name (fv exp)
  | True | False | Zero | Error(_)-> Set.empty
  | Succ(e) | Pred(e) | IsZero(e) -> fv e
  | If(e1,e2,e3) -> Set.union (fv e1) (fv e2) |> Set.union (fv e3)
  in let getFreeVariables = fv

let rec getNewVariableName fv baseName =
  if Set.contains baseName fv then getNewVariableName fv (String.Format("{0}'", baseName)) else baseName

let rec substitute exp1 name exp2 =
  printfn "substitute %A[%A:=%A]" exp1 name exp2
  let mapLambda _ name exp2 (name', e) =
    if name' = name then (name', e) else
    let fv1 = getFreeVariables e in
    let fv2 = getFreeVariables exp2 in
    if Set.contains name fv1 && Set.contains name' fv2 then
      let freshName = getNewVariableName (Set.union fv1 fv2) name' in
      (freshName, substitute (substitute e name' (Var freshName)) name exp2)
    else
      (name', substitute e name exp2)
    in
  match exp1 with
  | True | False | Zero | Error(_)-> exp1
  | Succ(e) -> Succ(substitute e name exp2)
  | Pred(e) -> Pred(substitute e name exp2)
  | IsZero(e) -> IsZero(substitute e name exp2)
  | If(e1,e2,e3) -> If(substitute e1 name exp2, substitute e2 name exp2, substitute e3 name exp2)
  | Var(name') -> if name' = name then exp2 else Var(name')
  | App(e1,e2) -> App(substitute e1 name exp2, substitute e2 name exp2)
  | Lambda(name', e) -> Lambda (mapLambda exp1 name exp2 (name', e))
  | Fix(name', e) -> Fix (mapLambda exp1 name exp2 (name', e))

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

let rec reduce exp =
  match exp with
  | Pred(e) -> reducePred e
  | Succ(e) -> reduceSucc e
  | IsZero(e) -> reduceIsZero e
  | If(e1,e2,e3) -> reduceIf e1 e2 e3
  | App(e1, e2) -> reduceApp e1 e2
  | Fix(name, e) -> reduceFix name e
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
  match exp1 with
  | Lambda(name, exp) -> substitute exp name exp2
  | _ -> App(reduce exp1, exp2)
and reduceFix name exp =
  substitute exp name (Fix(name,exp))

let rec parse env exp stack =
  printfn "[%A]%A : %A" stack exp (env "debug");
  match exp with
  | True -> True
  | False -> False
  | Zero -> Zero
  | Succ(e) -> 
    let v = parse env e (stack+1) in
    match v with
    | Zero -> Succ(Zero)
    | Succ(_) -> Succ(v)
    | Var(s) -> Succ(Var(s))
    | Error(e) ->
      match e with
        | Some(s) -> Error(Some(s))
        | _ -> Error(None)
    | _ -> Error(Some(String.Format("Error: Succ( {0} )", e)))
  | Pred(e) ->
    match parse env e (stack+1) with
    | Zero -> Zero
    | Succ(x) -> x
    | Var(s) -> Pred(Var(s))
    | Error(e) ->
      match e with
        | Some(s) -> Error(Some(s))
        | _ -> Error(None)
    | _ -> Error(Some(String.Format("Error: Pred( {0} )", e)))
  | IsZero(e) ->
    match parse env e (stack+1) with
    | Zero -> True
    | Succ(_) -> False
    | Var(s) -> IsZero(Var(s))
    | Error(e) ->
      match e with
        | Some(s) -> Error(Some(s))
        | _ -> Error(None)
    | _ -> Error(Some(String.Format("Error: IsZero( {0} )", e)))
  | Var(s) ->
    let v = env s in
    match v with
    | Error(_) -> Var(s)
    | _ -> v
  | If(e1,e2,e3) ->
    match parse env e1 (stack+1) with
      | True -> parse env e2 (stack+1)
      | False -> parse env e3 (stack+1)
      | Var(_) -> If(e1,e2,e3)
      | Succ(_) -> If(e1,e2,e3)
      | Pred(_) -> If(e1,e2,e3)
      | IsZero(_) -> If(e1,e2,e3)
      | Error(e) ->
        match e with
          | Some(s) -> Error(Some(s))
          | _ -> Error(None)
      | _ -> Error(Some(String.Format("Error: if {0} then {1} else {2}",e1,e2,e3)))
  | App(e1,e2) ->
    let v1 = parse env e1 (stack+1) in
    let v2 = parse env e2 (stack+1) in
    match v1 with
    | Lambda(name,exp) ->
      printfn "end parse of %A -> %A, start parse %A on %A = %A" e1 v1 exp name v2;
      parse (update env name (v2)) exp (stack+1)
    | Var(s) -> App(v1,v2)
    | Error(e) ->
      match e with
        | Some(s) -> Error(Some(s))
        | _ -> Error(None)
    | _ -> Error(Some(String.Format("Error: {0} {1}",e1,e2)))
  | Lambda(name,exp) -> Lambda(name, parse env exp (stack+1))
  // | Fix(name,exp) -> parse (update env name exp) exp (stack+1)
  // | Fix(name,exp) ->
    // let self = parse env exp (stack+1) in
    // parse (update env name self) self (stack+1)
  | Error(e) ->
    match e with
      | Some(s) -> Error(Some(s))
      | _ -> Error(None)
  | _ -> Error(None)

let test exp = printfn "%A" (parse emptyEnv exp 0)
let expression =
  App(
    Lambda(
      "x",
      If(
        IsZero(Var("x")),
        Zero,
        App(
          Lambda(
            "y",
            Succ(Succ(Var("y")))
          ),
          Var("x")
        )
      )
    ),
    Succ(Zero)
  )
// test expression // => Succ (Succ (Succ Zero))

let curryFuncExp =
  App(
    App(
      Lambda(
        "x",
        Lambda(
          "y",
          If(
            Var("x"),
            Succ(Var("y")),
            Zero
          )
        )
      ),
      True
    ),
    Succ(Zero)
  )
// test curryFuncExp // => Succ (Succ Zero)

let funcFuncExp =
  App(
    Lambda(
      "f",
      App(
        Var("f"),
        Zero
      )
    ),
    Lambda(
      "x",
      Succ(Var("x"))
    )
  )
// test funcFuncExp // => Succ Zero

// μf.λx.fx
let simpleFixExp =
  Fix(
    "f",
    Lambda(
      "x",
      App(
        Var("f"),
        Var("x")
      )
    )
  )
// test simpleFixExp

// μadd.λxy.(if iszero y then x else add x y)
let fixExp =
  Fix(
    "add",
    Lambda(
      "x",
      Lambda(
        "y",
        If(
          IsZero(Var("y")),
          Var("x"),
          App(
            App(
              Var("add"),
              Succ(Var("x"))
            ),
            Pred(Var("y"))
          )
        )
      )
    )
  )
// test fixExp
printfn "\n"

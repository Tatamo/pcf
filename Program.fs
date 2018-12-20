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
  | Lambda of string * Type * Exp
  // UNDONE
  // | Fix of string * Type * Exp

let expression =
  Lambda(
    "x",
    Num,
    If(
      IsZero(Var("x")),
      Zero,
      App(
        Lambda(
          "y",
          Num,
          Succ(Succ(Var("y")))
        ),
        Var("x")
      )
    )
  )

// 何かがおかしい
type Value =
  | Number of int
  | Bool of bool
  | Variable of string*Type*Value
  | Function of string*Exp
  | Error

type Env = string -> Value
let emptyEnv (s: string) = Error

let update env name value : Env =
  fun s -> if s = name then value else env(s);

let applyNum f expResult =
  match expResult with
    | Number(x) -> f x
    | _ -> Error

let rec parse env exp =
  match exp with
  | True -> Bool(true)
  | False -> Bool(false)
  | Zero -> Number(0)
  | Succ(e) -> applyNum (fun x -> Number(x+1)) (parse env e)
  | Pred(e) -> applyNum (fun x -> Number(max 0 (x-1))) (parse env e)
  | IsZero(e) -> applyNum (fun x -> if x = 0 then Bool(true) else Bool(false)) (parse env e)
  | Var(s) -> env s
  | If(e1,e2,e3) ->
    match parse env e1 with
      | Bool(b) -> if b then parse env e2 else parse env e3
      | _ -> Error
  // 以下未実装
  | App(e1, e2) ->
    match parse env e1 with
      | Function(name,exp) -> parse env e2
      | _ -> Error
  | Lambda(name,t,exp) -> Function(name,exp)
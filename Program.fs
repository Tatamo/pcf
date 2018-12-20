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
  | Error
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

type Env = string -> Exp
let emptyEnv (s: string) = Error

let update env name value : Env =
  fun s -> if s = name then value else env(s);

let rec parse env exp =
  match exp with
  | True -> True
  | False -> False
  | Zero -> Zero
  | Succ(e) -> 
    let v = parse env e in
    match v with
    | Zero -> Succ(Zero)
    | Succ(_) -> Succ(v)
    | _ -> Error
  | Pred(e) ->
    match parse env e with
    | Zero -> Zero
    | Succ(x) -> x
    | _ -> Error
  | IsZero(e) ->
    match parse env e with
    | Zero -> True
    | Succ(_) -> False
    | _ -> Error
  | Var(s) -> env s
  | If(e1,e2,e3) ->
    match parse env e1 with
      | True -> parse env e2
      | False -> parse env e3
      | _ -> Error
  // 以下未実装
  (*
  | App(e1, e2) ->
    match parse env e1 with
      | Function(name,exp) -> parse env e2
      | _ -> Error
  | Lambda(name,t,exp) -> Function(name,exp)
  *)
  | _ -> Error
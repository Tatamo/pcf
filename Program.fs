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
  | Error
  // UNDONE
  // | Fix of string * Type * Exp

type Env = string -> Exp
let emptyEnv (s: string) = Error

let update env name exp : Env =
  fun s -> if s = name then exp else env(s);

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
  | App(e1,e2) ->
    match parse env e1 with
    | Lambda(name,exp) -> parse (update env name (parse env e2)) exp
    | _ -> Error
  | Lambda(name,exp) -> Lambda(name,exp)
  | _ -> Error

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
printfn "%A" (parse emptyEnv expression);

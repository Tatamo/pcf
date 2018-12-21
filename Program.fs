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
  (*
    match parse env e1 with
    | Lambda(name,exp) -> parse (update env name (parse env e2)) exp
    | Error(e) ->
      match e with
        | Some(s) -> Error(Some(s))
        | _ -> Error(None)
    | _ -> Error(Some(String.Format("Error: {0} {1}",e1,e2)))
  *)
  | Lambda(name,exp) -> Lambda(name,parse env exp (stack+1))
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
test expression // => Succ (Succ (Succ Zero))

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

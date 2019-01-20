module SampleExpressions
open Type
open Exp

// (λx:num.if iszero x then zero else (λy:num.succ succ y) x) 0
let applyExp =
  App(
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
    ),
    Succ(Zero)
  )

// (λx:bool.λy:num.if x then succ y else 0) true 1
let curryFuncExp =
  App(
    App(
      Lambda(
        "x",
        Bool,
        Lambda(
          "y",
          Num,
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

// (λf:num->num.f 0)(λx:num.x+1)
let funcFuncExp =
  App(
    Lambda(
      "f",
      Func(Num,Num),
      App(
        Var("f"),
        Zero
      )
    ),
    Lambda(
      "x",
      Num,
      Succ(Var("x"))
    )
  )

// μf:num->num.λx:num.fx
let simpleFixExp =
  Fix(
    "f",
    Func(Num,Num),
    Lambda(
      "x",
      Num,
      App(
        Var("f"),
        Var("x")
      )
    )
  )

// μadd:num->num->num.λx:num.λy:num.(if iszero y then x else add x y)
let fixExp =
  Fix(
    "add",
    Func(Num,Func(Num,Num)),
    Lambda(
      "x",
      Num,
      Lambda(
        "y",
        Num,
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
// add 1 3
let fixExp' = App(App(fixExp, Succ Zero), Succ(Succ(Succ Zero)))

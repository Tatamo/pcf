module SampleExpressions
open Exp

// (λx.if iszero x then zero else (λy.succ succ y) x) 0
let applyExp =
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

// (λx.λy.if x then succ y else 0) true 1
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

// (λf.f 0)(λx.x)
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
// add 1 3
let fixExp' = App(App(fixExp, Succ Zero), Succ(Succ(Succ Zero)))

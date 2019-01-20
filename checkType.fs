module CheckType
open Type
open Exp

type Env = string -> Type option
let emptyEnv (_: string) = None: Type option
let updateEnv env name t: Env =
  fun key -> if key = name then (Some t) else env name

let rec checkType exp env =
  match exp with
  | Var(name) ->
    match env name with
    | Some(t) -> t
    | None -> TypeError
  | App(e1, e2) ->
    // Γ ⊦ M : σ->τ  Γ ⊦ N : σ ⇒ Γ ⊦ MN : τ
    let t1 = checkType e1 env in
    let t2 = checkType e2 env in
    match t1 with
    | Func(σ, τ) ->
      if σ = t2 then τ else TypeError
    | _ -> TypeError
  | Lambda(name,t,e) ->
    // Γ,x:σ ⊦ M : τ ⇒ Γ ⊦ λx:σ.M : σ->τ
    let τ = checkType e (updateEnv env name t) in
    Func(t, τ)
  | _ -> TypeError

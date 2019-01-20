module CheckType
open Type
open Exp

type Env = string -> Type option
let emptyEnv (_: string) = None: Type option
let updateEnv env name t: Env =
  fun key -> if key = name then (Some t) else env key

let rec checkType exp env =
  // printfn "%A, %A" (env "x") (env "y")
  match exp with
  | Var(name) ->
    // Γ ⊦ x : σ (if Γ(x)=σ)
    match env name with
    | Some(t) -> t
    | None -> TypeError
  | Zero -> Num
  | True | False -> Bool
  | Succ(e) ->
    // Γ ⊦ M : num ⇒ Γ ⊦ succ M : num
    match checkType e env with Num -> Num | _ -> TypeError
  | Pred(e) ->
    // Γ ⊦ M : num ⇒ Γ ⊦ pred M : num
    match checkType e env with Num -> Num | _ -> TypeError
  | IsZero(e) ->
    // Γ ⊦ M : num ⇒ Γ ⊦ iszero M : bool
    match checkType e env with Num -> Bool | _ -> TypeError
  | If(e1, e2, e3) ->
    // Γ ⊦ L : bool, Γ ⊦ M : σ, Γ ⊦ N : σ ⇒ Γ ⊦ if L then M else N : σ
    let t1 = checkType e1 env in
    let t2 = checkType e2 env in
    let t3 = checkType e3 env in
    match t1 with
    | Bool ->
      if t2 = t3 then t2 else TypeError
    | _ -> TypeError
  | App(e1, e2) ->
    // Γ ⊦ M : σ->τ, Γ ⊦ N : σ ⇒ Γ ⊦ MN : τ
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
  | Fix(name,t,e) ->
    // Γ,x:σ ⊦ M : σ ⇒ Γ ⊦ μx:σ.M : σ
    let σ = checkType e (updateEnv env name t) in
    if σ = t then σ else TypeError
  | _ -> TypeError

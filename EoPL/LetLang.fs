module LetLang

type Var = string

[<RequireQualifiedAccess>]
type Env =
    | Empty
    | Extend of var:Var * value:ExpVal * savedEnv:Env
    with
        static member apply searchVar = function
            | Empty -> failwith $"Variable {searchVar} not found"

            | Extend(savedVar, savedValue, _) when savedVar = searchVar -> savedValue
            | Extend(_, _, savedEnv) -> Env.apply searchVar savedEnv

// ExpVal = INT + BOOL
and ExpVal =
    | Num of int
    | Bool of bool
    with
        static member toNum = function | ExpVal.Num n -> n | _ -> failwith "Expected ExpVal.Num. Bad transform."
        static member toBool = function | ExpVal.Bool b -> b | _ -> failwith "Expected ExpVal.Bool. Bad transform."

and Exp =
    | Const of num:int
    | IsZero of exp1:Exp
    | If of exp1:Exp * exp2:Exp * exp3:Exp
    | Diff of exp1:Exp * exp2:Exp
    | Var of var:Var
    | Let of var:Var * exp1:Exp * body:Exp
    with
        static member valueOf env = function
            | Exp.Const n -> 
                ExpVal.Num n
            | Exp.IsZero exp1 ->
                let num = Exp.valueOf env exp1 |> ExpVal.toNum
                ExpVal.Bool (num = 0)
            | Exp.If(exp1, exp2, exp3) ->
                let bool = Exp.valueOf env exp1 |> ExpVal.toBool
                if bool then Exp.valueOf env exp2 else Exp.valueOf env exp3
            | Exp.Diff(exp1, exp2) ->
                let num1 = Exp.valueOf env exp1 |> ExpVal.toNum
                let num2 = Exp.valueOf env exp2 |> ExpVal.toNum
                ExpVal.Num (num1 - num2)
            | Exp.Var var ->
                Env.apply var env
            | Exp.Let(var, exp1, body) ->
                let value = Exp.valueOf env exp1
                let env1 = Env.Extend(var, value, env)
                Exp.valueOf env1 body

[<RequireQualifiedAccess>]
type Program =
    | A of Exp
    with
        static member valueOfProgram (A exp) = Exp.valueOf Env.Empty exp

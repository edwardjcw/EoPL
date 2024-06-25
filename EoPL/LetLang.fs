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

// ExpVal = INT + BOOL + LIST
and ExpVal =
    | Num of int
    | Bool of bool
    | List of ExpVal list                            // Exercise 3.9
    with
        static member toNum = function | ExpVal.Num n -> n | _ -> failwith "Expected ExpVal.Num. Bad transform."
        static member toBool = function | ExpVal.Bool b -> b | _ -> failwith "Expected ExpVal.Bool. Bad transform."
        static member toList = function | ExpVal.List l -> l | _ -> failwith "Expected ExpVal.List. Bad transform."     // Exercise 3.9

and Exp =
    | Const of num:int
    | IsZero of exp1:Exp
    | If of exp1:Exp * exp2:Exp * exp3:Exp
    | Diff of exp1:Exp * exp2:Exp
    | Var of var:Var
    | Let of var:Var * exp1:Exp * body:Exp
    | Minus of exp:Exp                                  // Exercise 3.6
    | Add of exp1:Exp * exp2:Exp                        // Exercise 3.7
    | Mult of exp1:Exp * exp2:Exp                       // Exercise 3.7
    | IntDiv of exp1:Exp * exp2:Exp                     // Exercise 3.7
    | IsEqual of exp1:Exp * exp2:Exp                    // Exercise 3.8
    | IsGreater of exp1:Exp * exp2:Exp                  // Exercise 3.8
    | IsLess of exp1:Exp * exp2:Exp                     // Exercise 3.8
    | Cons of head:Exp * tail:Exp                       // Exercise 3.9
    | Car of exp:Exp                                    // Exercise 3.9
    | Cdr of exp:Exp                                    // Exercise 3.9
    | IsNull of exp:Exp                                 // Exercise 3.9
    | EmptyList                                         // Exercise 3.9
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
            | Exp.Minus exp ->                      // Exercise 3.6
                let num = Exp.valueOf env exp |> ExpVal.toNum
                ExpVal.Num (-num)
            | Exp.Add(exp1, exp2) ->                // Exercise 3.7
                let num1 = Exp.valueOf env exp1 |> ExpVal.toNum
                let num2 = Exp.valueOf env exp2 |> ExpVal.toNum
                ExpVal.Num (num1 + num2)
            | Exp.Mult(exp1, exp2) ->               // Exercise 3.7
                let num1 = Exp.valueOf env exp1 |> ExpVal.toNum
                let num2 = Exp.valueOf env exp2 |> ExpVal.toNum
                ExpVal.Num (num1 * num2)
            | Exp.IntDiv(exp1, exp2) ->             // Exercise 3.7
                let num1 = Exp.valueOf env exp1 |> ExpVal.toNum
                let num2 = Exp.valueOf env exp2 |> ExpVal.toNum
                ExpVal.Num (num1 / num2)
            | Exp.IsEqual(exp1, exp2) ->            // Exercise 3.8
                let num1 = Exp.valueOf env exp1 |> ExpVal.toNum
                let num2 = Exp.valueOf env exp2 |> ExpVal.toNum
                ExpVal.Bool (num1 = num2)
            | Exp.IsGreater(exp1, exp2) ->          // Exercise 3.8
                let num1 = Exp.valueOf env exp1 |> ExpVal.toNum
                let num2 = Exp.valueOf env exp2 |> ExpVal.toNum
                ExpVal.Bool (num1 > num2)
            | Exp.IsLess(exp1, exp2) ->             // Exercise 3.8
                let num1 = Exp.valueOf env exp1 |> ExpVal.toNum
                let num2 = Exp.valueOf env exp2 |> ExpVal.toNum
                ExpVal.Bool (num1 < num2)
            | Exp.Cons(head, tail) ->               // Exercise 3.9
                let headVal = Exp.valueOf env head
                let tailVal = Exp.valueOf env tail
                ExpVal.List (headVal::(tailVal |> ExpVal.toList))
            | Exp.Car exp ->                        // Exercise 3.9 
                let listVal = Exp.valueOf env exp |> ExpVal.toList
                listVal |> List.head
            | Exp.Cdr exp ->                        // Exercise 3.9 
                let listVal = Exp.valueOf env exp |> ExpVal.toList
                listVal |> List.tail |> ExpVal.List
            | Exp.IsNull exp ->                     // Exercise 3.9 
                let listVal = Exp.valueOf env exp |> ExpVal.toList
                listVal |> List.isEmpty |> ExpVal.Bool
            | Exp.EmptyList ->                      // Exercise 3.9 
                ExpVal.List List.empty

[<RequireQualifiedAccess>]
type Program =
    | A of Exp
    with
        static member valueOfProgram (A exp) = Exp.valueOf Env.Empty exp

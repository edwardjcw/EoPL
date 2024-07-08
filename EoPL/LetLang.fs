module LetLang

type Var = string
type Vars = Var list

[<RequireQualifiedAccess>]
type Env =
    | Empty
    | Extend of var:Var * value:DenVal * savedEnv:Env
    | ExtendRec of (Var * DenVal) list * savedEnv:Env  // Exercise 3.33 modified
    with
        static member apply searchVar = function
            | Empty -> failwith $"Variable {searchVar} not found"

            | Extend(savedVar, savedValue, _) when savedVar = searchVar -> savedValue
            | Extend(_, _, savedEnv) -> Env.apply searchVar savedEnv

            | ExtendRec(procs, savedEnv) ->     // Exercise 4.19 modified
                procs
                |> List.tryFind (fun (pName, _) -> pName = searchVar)
                |> Option.map snd
                |> function Some(x) -> x | None -> Env.apply searchVar savedEnv

and Proc =
    | Procedure of Vars * Exp * Env                 // Exercise 3.21 modified
    with 
        static member applyProcedure (Proc.Procedure(vars, body, savedEnv)) (args : ExpVal list) =
            let env1 = args |> List.zip vars |> List.fold (fun acc (var, value) -> Env.Extend(var, Store.newRef value, acc)) savedEnv
            Exp.valueOf env1 body

// explicit ref store (Section 4.2)
and Store() =
    static let mutable store = Map<int, ExpVal> []
    static let mutable nextLoc = 0
    static member newRef value =
        let loc = nextLoc
        nextLoc <- nextLoc + 1
        store <- store.Add(loc, value)
        DenVal.Ref (ExpVal.Num loc)
    static member deRef ref =
        let key = ExpVal.toNum (DenVal.toRef ref)
        store.[key]
    static member setRef ref value =
        let key = ExpVal.toNum (DenVal.toRef ref)
        if not (store.ContainsKey(key)) then failwith "setRef: invalid reference"
        else
            store <- store.Add(key, value)
            ExpVal.Unit

// ExpVal = INT + BOOL + LIST + PROC //+ REF
and ExpVal =
    | Num of int
    | Bool of bool
    | List of ExpVal list                            // Exercise 3.9
    | Proc of Proc                                   // pg 79
    //| Ref of ExpVal                                  // Section 4.2
    | Unit                                           // Section 4.2
    with
        static member toNum = function | ExpVal.Num n -> n | _ -> failwith "Expected ExpVal.Num. Bad transform."
        static member toBool = function | ExpVal.Bool b -> b | _ -> failwith "Expected ExpVal.Bool. Bad transform."
        static member toList = function | ExpVal.List l -> l | _ -> failwith "Expected ExpVal.List. Bad transform."     // Exercise 3.9
        static member toProc = function | ExpVal.Proc p -> p | _ -> failwith "Expected ExpVal.Proc. Bad transform."    // pg. 79
        //static member toRef = function | ExpVal.Ref r -> r | _ -> failwith "Expected ExpVal.Ref. Bad transform."      // Section 4.2

// DenVal = Ref(ExpVal) + ExpVal            // Exercise 4.20 modified
and DenVal =
    | Ref of ExpVal
    | ExpVal of ExpVal
    with
        static member toRef = function | DenVal.Ref r -> r | _ -> failwith "Expected DenVal.Ref. Bad transform."
        static member toExpVal = function | DenVal.ExpVal e -> e | _ -> failwith "Expected DenVal.ExpVal. Bad transform."

and Exp =
    | Const of num:int
    | IsZero of exp1:Exp
    | IsOne of exp1:Exp
    | If of exp1:Exp * exp2:Exp * exp3:Exp
    | Cond of (Exp * Exp) list                          // Exercise 3.12
    | Or of Exp list
    | Diff of exp1:Exp * exp2:Exp
    | Var of var:Var
    | Let of (Var * Exp) list * body:Exp                // Exercise 3.16 modified
    | LetStar of (Var * Exp) list * body:Exp            // Exercise 3.17
    | Unpack of Vars * Exp * body:Exp                   // Exercise 3.18
    | LetMutable of (Var * Exp) list * body:Exp         // Exercise 4.20
    | SetDynamic of Var * Exp * body:Exp                // Exercise 4.21
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
    | List of Exp list                                  // Exercise 3.10
    | Proc of Vars * body:Exp                           // Exercise 3.21 modified
    | Call of rator:Exp * rands:Exp list                // Exercise 3.21 modified
    | LetProc of Var * Vars * Exp * Exp                 // Exercise 3.21 modified
    | LetRec of (Var * Vars * Exp) list * Exp           // Exercise 3.33 modified
    //| NewRef of exp:Exp                                 // Section 4.2
    //| DeRef of exp:Exp                                  // Section 4.2
    //| SetRef of exp1:Exp * exp2:Exp                     // Section 4.2
    | Assign of var:Var * exp:Exp                       // Section 4.3
    | Begin of Exp list                                 // Exercise 4.10
    with
        static member valueOf env = function
            | Exp.Const n -> 
                ExpVal.Num n
            | Exp.IsZero exp1 ->
                let num = Exp.valueOf env exp1 |> ExpVal.toNum
                ExpVal.Bool (num = 0)
            | Exp.IsOne exp1 ->                          
                let num = Exp.valueOf env exp1 |> ExpVal.toNum
                ExpVal.Bool (num = 1)
            | Exp.If(exp1, exp2, exp3) ->
                let bool = Exp.valueOf env exp1 |> ExpVal.toBool
                if bool then Exp.valueOf env exp2 else Exp.valueOf env exp3
            | Exp.Cond conds ->                         // Exercise 3.12 
                conds
                |> List.map (fun (cond, exp) -> (Exp.valueOf env cond |> ExpVal.toBool, exp))
                |> List.tryFind (fun (bool, _) -> bool)
                |> Option.map (fun (_, exp) -> Exp.valueOf env exp)
                |> function Some(x) -> x | None -> failwith "No true condition found." 
            | Exp.Or exps ->
                let rec orExp = function
                    | [] -> ExpVal.Bool false
                    | exp::exps -> 
                        let bool = Exp.valueOf env exp |> ExpVal.toBool
                        if bool then ExpVal.Bool true else orExp exps
                orExp exps
            | Exp.Diff(exp1, exp2) ->
                let num1 = Exp.valueOf env exp1 |> ExpVal.toNum
                let num2 = Exp.valueOf env exp2 |> ExpVal.toNum
                ExpVal.Num (num1 - num2)
            | Exp.Var var ->                        // Section 4.3 modified
                let value = Env.apply var env
                match value with
                | DenVal.Ref _ -> Store.deRef value
                | DenVal.ExpVal value -> value
            | Exp.Let(exps, body) ->                // Exercise 3.16 modified
                let varsValues = exps |> List.map (fun (var, exp) -> (var, Exp.valueOf env exp))
                let env1 = varsValues |> List.fold (fun acc (var, value) -> Env.Extend(var, DenVal.ExpVal value, acc)) env
                Exp.valueOf env1 body
            | Exp.LetStar(exps, body) ->            // Exercise 3.17 
                let env1 = exps |> List.fold (fun acc (var, exp) -> Env.Extend(var, Exp.valueOf acc exp |> DenVal.ExpVal, acc)) env
                Exp.valueOf env1 body
            | Exp.Unpack(vars, exp, body) ->        // Exercise 3.18 
                let listVal = Exp.valueOf env exp |> ExpVal.toList
                if List.length vars <> List.length listVal then failwith "Unpack: vars and listVal have different lengths."
                let env1 = listVal |> List.zip vars |> List.fold (fun acc (var, value) -> Env.Extend(var, DenVal.ExpVal value, acc)) env
                Exp.valueOf env1 body
            | Exp.LetMutable(exps, body) ->         // Exercise 4.20
                let varsValues = exps |> List.map (fun (var, exp) -> (var, Exp.valueOf env exp))
                let env1 = varsValues |> List.fold (fun acc (var, value) -> Env.Extend(var, Store.newRef value, acc)) env
                Exp.valueOf env1 body
            | Exp.SetDynamic(var, exp, body) ->     // Exercise 4.21
                let loc = Env.apply var env
                let retainOriginal = Store.deRef loc
                let value = Exp.valueOf env exp
                Store.setRef loc value |> ignore
                let result = Exp.valueOf env body
                Store.setRef loc retainOriginal |> ignore
                result
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
            | Exp.List exps ->                      // Exercise 3.10 
                let listVal = exps |> List.map (Exp.valueOf env)
                ExpVal.List listVal
            | Exp.Proc (vars, body) ->              // Exercise 3.21 modified
                ExpVal.Proc (Proc.Procedure(vars, body, env))
            | Exp.Call (rator, rands) ->            // Exercise 3.21 modified
                let proc = Exp.valueOf env rator |> ExpVal.toProc
                let args = rands |> List.map (Exp.valueOf env)
                Proc.applyProcedure proc args
            | Exp.LetProc (var, procVars, procBody, body) -> // Exercise 3.21 modified
                let proc = Proc.Procedure(procVars, procBody, env)
                let env1 = Env.Extend(var, Store.newRef (ExpVal.Proc proc), env)
                Exp.valueOf env1 body
            | Exp.LetRec (procs, letrecBody) -> // Exercise 4.19 modified
                let (env1, placeholderRefs) = 
                    let placeholders = procs |> List.map (fun (pName, _, _) -> (pName, Store.newRef ExpVal.Unit))
                    (Env.ExtendRec(placeholders, env), placeholders)
                procs |> List.iter2 (fun (_, ref) (_, bVars, body) -> Store.setRef ref (ExpVal.Proc (Proc.Procedure(bVars, body, env1))) |> ignore) placeholderRefs
                Exp.valueOf env1 letrecBody
            //| Exp.NewRef exp ->                    // Section 4.2
            //    let value = Exp.valueOf env exp
            //    Store.newRef value
            //| Exp.DeRef exp ->                     // Section 4.2
            //    let ref = Exp.valueOf env exp
            //    Store.deRef ref
            //| Exp.SetRef (exp1, exp2) ->           // Section 4.2 
            //    let ref = Exp.valueOf env exp1
            //    let value = Exp.valueOf env exp2
            //    Store.setRef ref value
            | Exp.Assign (var, exp) ->               // Section 4.3
                let loc = Env.apply var env
                let value = Exp.valueOf env exp
                Store.setRef loc value
            | Exp.Begin exps ->                    // Exercise 4.10
                exps |> List.map (Exp.valueOf env) |> List.last

[<RequireQualifiedAccess>]
type Program =
    | A of Exp
    with
        static member valueOfProgram (A exp) = Exp.valueOf Env.Empty exp

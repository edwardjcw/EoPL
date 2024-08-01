module LetLang

type Var = string
type Vars = Var list

[<RequireQualifiedAccess>]
type Env =
    | Empty
    | Extend of var:Var * value:DenVal * savedEnv:Env
    | ExtendStar of vars:Vars * values:DenVal list * savedEnv:Env
    | ExtendRec of (Var * DenVal) list * savedEnv:Env  // Exercise 3.33 modified
    | ExtendWithSelfAndSuper of self:Obj * superName:Var option * savedEnv:Env  // Section 9.4.2
    with
        static member apply searchVar = function
            | Empty -> failwith $"Variable {searchVar} not found"

            | Extend(savedVar, savedValue, _) when savedVar = searchVar -> savedValue
            | Extend(_, _, savedEnv) -> Env.apply searchVar savedEnv

            | ExtendStar(vars, values, savedEnv) -> 
                vars
                |> List.tryFind (fun v -> v = searchVar)
                |> Option.map (fun v -> values.[vars |> List.findIndex (fun x -> x = v)])
                |> function Some(x) -> x | None -> Env.apply searchVar savedEnv

            | ExtendRec(procs, savedEnv) ->     // Exercise 4.19 modified
                procs
                |> List.tryFind (fun (pName, _) -> pName = searchVar)
                |> Option.map snd
                |> function Some(x) -> x | None -> Env.apply searchVar savedEnv

            | ExtendWithSelfAndSuper(self, superName, savedEnv) -> 
                match searchVar, superName with
                | "%self", _ -> DenVal.ExpVal (ExpVal.Obj self)
                | "%super", Some(superName) -> DenVal.ExpVal (ExpVal.Obj (Obj.newObj superName))
                | _ -> Env.apply searchVar savedEnv

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
    static member initialize () = store <- Map.empty
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

and ClassEnv() =                        // Section 9.4.3
    static let mutable classEnv = Map<Var, Class> []
    static let appendFieldNames (superFields: Vars) (newFields: Vars) = 
        let rec looper superFields1 result = 
            match superFields1 with
            | [] -> (result |> List.rev) @ newFields
            | f::rest -> 
                if newFields |> List.exists (fun x -> x = f) then 
                    let freshIdentifier = f + "%shadowed"
                    looper rest (freshIdentifier::result)
                else looper rest (f::result)
        looper superFields []
    static let initializeClassDecl (ClassDecl(className, superName, fieldNames, methods)) =
        let fieldNames1 = appendFieldNames (Class.toFieldNames (ClassEnv.lookup superName)) fieldNames
        let superMethodEnv = Class.toMethodEnv (ClassEnv.lookup superName)
        let newMethodEnv = MethodEnv().initializeMethodEnv methods className fieldNames1        // Exercise 9.5
        let mergedMethodEnv = MethodEnv.mergeMethodEnv superMethodEnv newMethodEnv
        let newClass = Class.Class(Some superName, fieldNames1, mergedMethodEnv)
        classEnv <- classEnv.Add(className, newClass)

    static member addClass className classDef = classEnv <- classEnv.Add(className, classDef)
    static member lookup className = 
        if classEnv.ContainsKey(className) then classEnv.[className]
        else failwith $"Class {className} is unknown"
    static member initializeClassEnv classDecls = 
        classEnv <- Map.empty
        let superObj = Class.Class(None, [], MethodEnv())
        classEnv <- classEnv.Add("object", superObj)
        classDecls |> List.iter initializeClassDecl

and MethodEnv() =               // Section 9.4.4
    let mutable methodEnv = Map<Var, Method> []
    member _.add methodName method = methodEnv <- methodEnv.Add(methodName, method)
    member _.copyOfEnv = 
        let mutable newEnv = Map<Var, Method> []
        methodEnv |> Map.iter (fun k v -> newEnv <- newEnv.Add(k, v))
        newEnv
    member this.initializeMethodEnv methods className (fieldNames: Vars) =
        methodEnv <- Map.empty
        methods |> List.iter (fun (MethodDecl(access, methodName, parameters, body)) -> 
            let newMethod = Method.Method(access, parameters, body, className, fieldNames)      // Exercise 9.5
            methodEnv <- methodEnv.Add(methodName, newMethod))
        this
    member _.tryGetMethod methodName =
        if methodEnv.ContainsKey(methodName) then Some(methodEnv.[methodName])
        else None
    static member findMethod className methodName =
        let classDef = ClassEnv.lookup className
        let (methodEnv: MethodEnv) = Class.toMethodEnv classDef
        match methodEnv.tryGetMethod methodName with
        | Some(method) -> method
        | None -> failwith $"Method {methodName} not found in class {className}"
    static member mergeMethodEnv superMethodEnv (newMethodEnv : MethodEnv) =
        let mergedMethodEnv = MethodEnv()
        superMethodEnv.copyOfEnv |> Map.iter (fun k v -> mergedMethodEnv.add k v)
        newMethodEnv.copyOfEnv |> Map.iter (fun k v -> mergedMethodEnv.add k v)
        mergedMethodEnv

// ExpVal = INT + BOOL + LIST + PROC + MutPair + ArrVal + REF + Thunk + Obj + Str + UNIT
and ExpVal =
    | Num of int
    | Bool of bool
    | List of ExpVal list                            // Exercise 3.9
    | Proc of Proc                                   // pg 79
    | Ref of ExpVal                                     // Exercise 4.35
    | MutPair of MutPair                             // Section 4.4
    | ArrVal of ArrVal                               // Exercise 4.29
    | Thunk of Thunk                                 // Section 4.5.2
    | Obj of Obj                                     // Section 9.3
    | Str of string
    | Unit                                           // Section 4.2
    with
        static member toNum = function | ExpVal.Num n -> n | _ -> failwith "Expected ExpVal.Num. Bad transform."
        static member toBool = function | ExpVal.Bool b -> b | _ -> failwith "Expected ExpVal.Bool. Bad transform."
        static member toList = function | ExpVal.List l -> l | _ -> failwith "Expected ExpVal.List. Bad transform."     // Exercise 3.9
        static member toProc = function | ExpVal.Proc p -> p | _ -> failwith "Expected ExpVal.Proc. Bad transform."    // pg. 79
        static member toRef = function | ExpVal.Ref r -> r | _ -> failwith "Expected ExpVal.Ref. Bad transform."      // Exercise 4.35
        static member toMutPair = function | ExpVal.MutPair p -> p | _ -> failwith "Expected ExpVal.MutPair. Bad transform." // Section 4.4
        static member toArrVal = function | ExpVal.ArrVal a -> a | _ -> failwith "Expected ExpVal.ArrVal. Bad transform." // Exercise 4.29
        static member toThunk = function | ExpVal.Thunk t -> t | _ -> failwith "Expected ExpVal.Thunk. Bad transform." // Section 4.5.2
        static member toObj = function | ExpVal.Obj o -> o | _ -> failwith "Expected ExpVal.Obj. Bad transform." // Section 9.3
        static member toStr = function | ExpVal.Str s -> s | _ -> failwith "Expected ExpVal.Str. Bad transform."

        static member toDenValRef = function | ExpVal.Ref r -> DenVal.Ref r | _ -> failwith "Expected ExpVal.Ref. Bad transform."

// DenVal = Ref(ExpVal) + ExpVal            // Exercise 4.20 modified
and DenVal =
    | Ref of ExpVal
    | ExpVal of ExpVal
    with
        static member toRef = function | DenVal.Ref r -> r | _ -> failwith "Expected DenVal.Ref. Bad transform."
        static member toExpVal = function | DenVal.ExpVal e -> e | _ -> failwith "Expected DenVal.ExpVal. Bad transform."

// MutPair = Ref(ExpVal)       // Section 4.4.2
and MutPair =
    | Ref of ExpVal
    with
        static member toRef = function | MutPair.Ref l -> l | _ -> failwith "Expected MutPair.Ref. Bad transform."

        static let rightRef pair = (pair |> MutPair.toRef |> ExpVal.toNum) + 1 |> ExpVal.Num |> DenVal.Ref
        static member makePair expVal1 expVal2 = 
            let ref1 = Store.newRef expVal1
            Store.newRef expVal2 |> ignore
            MutPair.Ref (ref1 |> DenVal.toRef)    // the second reference will be based on ref1
        static member left pair = Store.deRef (pair |> MutPair.toRef |> DenVal.Ref)
        static member right pair = 
            Store.deRef (rightRef pair)
        static member setLeft pair expVal = Store.setRef (pair |> MutPair.toRef |> DenVal.Ref) expVal
        static member setRight pair expVal = Store.setRef (rightRef pair) expVal

// ArrVal = (Ref(ExpVal)) array       // Exercise 4.29
and ArrVal =
    | Ref of ExpVal     // keep only the ref from the first element, which is length (Exercise 4.30)
    with
        static member toRef = function | ArrVal.Ref r -> r | _ -> failwith "Expected ArrVal.Ref. Bad transform."

        static let index arrVal expVal = 
            let index = (ExpVal.toNum expVal) + 1
            let length = ExpVal.toNum (ArrVal.length arrVal)
            if index < 1 || index > length then failwith "Array index out of bounds."
            (arrVal |> ArrVal.toRef |> ExpVal.toNum) + index |> ExpVal.Num |> DenVal.Ref
        static member make expVal1 expVal2 =
            let length = (ExpVal.toNum expVal1) + 1 // extra to accomodate saving the length as the first element
            let ref = List.init length (fun i -> if i = 0 then Store.newRef expVal1 else Store.newRef expVal2) |> List.head
            ArrVal.Ref (ref |> DenVal.toRef)
        static member ref arrVal expVal =
            Store.deRef (index arrVal expVal)
        static member set arrVal expVal1 expVal2 =
            Store.setRef (index arrVal expVal1) expVal2
        static member length arrVal =               // Exercise 4.30
            Store.deRef (arrVal |> ArrVal.toRef |> DenVal.Ref)

and Thunk =                             // Section 4.5.2
    | Thunk of Exp * Env

and Obj =                               // Section 9.3
    | Obj of className:Var * fields:DenVal list
    with 
        static member toClassName = function | Obj(className, _) -> className | _ -> failwith "Expected Obj. Bad transform."
        static member toFields = function | Obj(_, fields) -> fields | _ -> failwith "Expected Obj. Bad transform."

        static member newObj className =
            let fieldNames = Class.toFieldNames (ClassEnv.lookup className)
            let fields = fieldNames |> List.map (fun f -> Store.newRef (ExpVal.List [ExpVal.Str "uninitializedField"; ExpVal.Str f]))
            Obj.Obj(className, fields)
        static member instanceOf obj className =
            let rec looper className1 =
                if className = className1 then true
                else
                    let classDef = ClassEnv.lookup className1
                    match Class.toSuperName classDef with
                    | Some(superName) -> looper superName
                    | None -> false
            looper (Obj.toClassName obj)
        static member fieldRef (Obj.Obj(className, fields)) fieldName =  // Exercise 9.8
            let fieldNames = Class.toFieldNames (ClassEnv.lookup className)
            let index = fieldNames |> List.findIndex (fun f -> f = fieldName)
            Store.deRef (fields.[index])
        static member fieldSet (Obj.Obj(className, fields)) fieldName value =  // Exercise 9.8
            let fieldNames = Class.toFieldNames (ClassEnv.lookup className)
            let index = fieldNames |> List.findIndex (fun f -> f = fieldName)
            Store.setRef (fields.[index]) value

and Method =                            // Section 9.3
    | Method of access:Access * Vars * body:Exp * className:Var * fieldNames:Vars
    with
        static member applyMethod (Method.Method(_, vars, body, className, fieldNames)) self args =
            let env1 = Env.ExtendStar(fieldNames, (Obj.toFields self), Env.Empty)
            let superName = Class.toSuperName (ClassEnv.lookup className)  // Exercise 9.5
            let env2 = Env.ExtendWithSelfAndSuper(self, superName, env1)
            let env3 = Env.ExtendStar(vars, (args |> List.map Store.newRef), env2)
            Exp.valueOf env3 body

and Class =                             // Section 9.4.3
    | Class of superName:Var option * fieldNames:Vars * methodEnv:MethodEnv
    with
        static member toSuperName = function | Class(superName, _, _) -> superName | _ -> failwith "Expected Class. Bad transform."
        static member toFieldNames = function | Class(_, fieldNames, _) -> fieldNames | _ -> failwith "Expected Class. Bad transform."
        static member toMethodEnv = function | Class(_, _, methodEnv) -> methodEnv | _ -> failwith "Expected Class. Bad transform."

and Access =                            // Exercise 9.11
    | Public
    | Protected
    | Private

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
    | NewPair of exp1:Exp * exp2:Exp                    // Section 4.4
    | Left of exp:Exp                                   // Section 4.4
    | Right of exp:Exp                                  // Section 4.4
    | SetLeft of exp1:Exp * exp2:Exp                    // Section 4.4
    | SetRight of exp1:Exp * exp2:Exp                   // Section 4.4
    | NewArray of exp1:Exp * exp2:Exp                   // Exercise 4.29
    | ArrayRef of exp1:Exp * exp2:Exp                   // Exercise 4.29
    | ArraySet of exp1:Exp * exp2:Exp * exp3:Exp        // Exercise 4.29
    | ArrayLength of exp:Exp                            // Exercise 4.30
    | Ref of var:Var                                    // Exercise 4.35
    | DeRef of exp:Exp                                  // Exercise 4.35
    | SetRef of exp1:Exp * exp2:Exp                     // Exercise 4.35
    | Lazy of exp:Exp                                   // Section 4.5.2
    | New of className:Var * rands:Exp list             // Section 9.3
    | Send of obj:Exp * methodName:Var * rands:Exp list // Section 9.3
    | Super of methodName:Var * rands:Exp list          // Section 9.3
    | Self                                              // Section 9.3
    | InstanceOf of obj:Exp * className:Var             // Exercise 9.6
    | FieldRef of obj:Exp * fieldName:Var               // Exercise 9.8
    | FieldSet of obj:Exp * fieldName:Var * exp:Exp     // Exercise 9.8
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
            | Exp.Var var ->                        // Section 4.5.2 modified
                let value = Env.apply var env
                match value with
                | DenVal.Ref _ -> 
                    match Store.deRef value with
                    | ExpVal.Thunk t -> 
                        let v = Exp.valueOfThunk t
                        Store.setRef value v |> ignore
                        v
                    | v -> v
                | DenVal.ExpVal value ->
                    match value with
                    | ExpVal.Thunk t -> Exp.valueOfThunk t
                    | _ -> value
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
            | Exp.NewPair (exp1, exp2) ->          // Section 4.4
                let left = Exp.valueOf env exp1
                let right = Exp.valueOf env exp2
                ExpVal.MutPair (MutPair.makePair left right)
            | Exp.Left exp ->                       // Section 4.4
                let pair = Exp.valueOf env exp |> ExpVal.toMutPair
                MutPair.left pair
            | Exp.Right exp ->                      // Section 4.4
                let pair = Exp.valueOf env exp |> ExpVal.toMutPair
                MutPair.right pair
            | Exp.SetLeft (exp1, exp2) ->           // Section 4.4
                let pair = Exp.valueOf env exp1 |> ExpVal.toMutPair
                let value = Exp.valueOf env exp2
                MutPair.setLeft pair value
            | Exp.SetRight (exp1, exp2) ->          // Section 4.4
                let pair = Exp.valueOf env exp1 |> ExpVal.toMutPair
                let value = Exp.valueOf env exp2
                MutPair.setRight pair value
            | Exp.NewArray (exp1, exp2) ->          // Exercise 4.29
                let length = Exp.valueOf env exp1
                let value = Exp.valueOf env exp2
                ExpVal.ArrVal (ArrVal.make length value)
            | Exp.ArrayRef (exp1, exp2) ->          // Exercise 4.29
                let arrVal = Exp.valueOf env exp1 |> ExpVal.toArrVal
                let index = Exp.valueOf env exp2
                ArrVal.ref arrVal index
            | Exp.ArraySet (exp1, exp2, exp3) ->    // Exercise 4.29
                let arrVal = Exp.valueOf env exp1 |> ExpVal.toArrVal
                let index = Exp.valueOf env exp2
                let value = Exp.valueOf env exp3
                ArrVal.set arrVal index value
            | Exp.ArrayLength exp ->                // Exercise 4.30
                let arrVal = Exp.valueOf env exp |> ExpVal.toArrVal
                ArrVal.length arrVal
            | Exp.Ref var ->                        // Exercise 4.35
                let value = Env.apply var env
                match value with
                | DenVal.Ref loc -> loc |> ExpVal.Ref
                | _ -> failwith "Expected reference."
            | Exp.DeRef exp ->                      // Exercise 4.35
                let loc = Exp.valueOf env exp |> ExpVal.toDenValRef
                Store.deRef loc
            | Exp.SetRef (exp1, exp2) ->            // Exercise 4.35 
                let loc = Exp.valueOf env exp1 |> ExpVal.toDenValRef
                let value = Exp.valueOf env exp2
                Store.setRef loc value
            | Exp.Lazy exp ->                       // Section 4.5.2
                ExpVal.Thunk (Thunk.Thunk (exp, env))
            | Exp.New (className, rands) ->         // Section 9.3
                let args = rands |> List.map (Exp.valueOf env)
                let obj = Obj.newObj className
                let method = MethodEnv.findMethod className "initialize"
                match method with   // Exercise 9.11
                | Method.Method(Access.Public, _, _, _, _) ->
                    Method.applyMethod method obj args |> ignore
                    ExpVal.Obj obj
                | _ -> failwith "initialize method must be public"
            | Exp.Send (obj, methodName, rands) ->  // Section 9.3
                let objVal = Exp.valueOf env obj |> ExpVal.toObj
                let className = Obj.toClassName objVal
                let method = MethodEnv.findMethod (Obj.toClassName objVal) methodName
                match (obj, method) with  // Exercise 9.11
                | _, Method.Method(_, _, _, hostname, _) when hostname = className -> 
                    let args = rands |> List.map (Exp.valueOf env)
                    Method.applyMethod method objVal args
                | Exp.Self, Method.Method(Access.Protected, _, _, _, _) | _, Method.Method(Access.Public, _, _, _, _) ->
                    let args = rands |> List.map (Exp.valueOf env)
                    Method.applyMethod method objVal args
                | _ -> failwith "Method must be public"
            | Exp.Super (methodName, rands) ->      // Section 9.3
                let self = Env.apply "%self" env |> DenVal.toExpVal |> ExpVal.toObj
                let super = Env.apply "%super" env |> DenVal.toExpVal |> ExpVal.toObj |> Obj.toClassName
                let method = MethodEnv.findMethod super methodName
                let args = rands |> List.map (Exp.valueOf env)
                Method.applyMethod method self args
            | Exp.Self ->                          // Section 9.3
                let self = Env.apply "%self" env |> DenVal.toExpVal
                self
            | Exp.InstanceOf (obj, className) ->    // Exercise 9.6
                let objVal = Exp.valueOf env obj |> ExpVal.toObj
                ExpVal.Bool (Obj.instanceOf objVal className)
            | Exp.FieldRef (obj, fieldName) ->     // Exercise 9.8
                let objVal = Exp.valueOf env obj |> ExpVal.toObj
                Obj.fieldRef objVal fieldName
            | Exp.FieldSet (obj, fieldName, exp) -> // Exercise 9.8 
                let objVal = Exp.valueOf env obj |> ExpVal.toObj
                let value = Exp.valueOf env exp
                Obj.fieldSet objVal fieldName value
        static member valueOfThunk (Thunk.Thunk (exp, env)) = Exp.valueOf env exp

// Section 9.3
and MethodDecl =
    | MethodDecl of access:Access * methodName:Var * parameters:Vars * body:Exp

// Section 9.3
and ClassDecl =
    | ClassDecl of className:Var * superName:Var * fieldNames:Vars * methods:MethodDecl list

[<RequireQualifiedAccess>]
type Program =
    | A of ClassDecl list * Exp
    with
        static member valueOfProgram (A(classDecls, exp)) = 
            Store.initialize()
            ClassEnv.initializeClassEnv classDecls
            Exp.valueOf Env.Empty exp

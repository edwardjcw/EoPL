module Parser

open FParsec
open LetLang

let str = pstring
let ws = spaces
let strWs s = str s >>. ws
//let skipStrWs s = skipString s >>. ws
let pipe6 p1 p2 p3 p4 p5 p6 f = pipe4 p1 p2 p3 (tuple3 p4 p5 p6) (fun a b c (d, e, g) -> f a b c d e g)

let pinteger : Parser<int, unit> = pint32 |>> int
let constExp : Parser<Exp, unit> = pinteger |>> Exp.Const
let pvar : Parser<Var, unit> = many1Satisfy2 isLetter (System.Char.IsLetterOrDigit) |>> string
let varExp : Parser<Exp, unit> = pvar |>> Exp.Var

let pexp, pexpRef = createParserForwardedToRef<Exp, unit>()

let letExp = skipString "let" >>. ws >>. many1 (skipString "[" >>. ws >>. pvar .>> ws .>> skipString "=" .>> ws .>>. pexp .>> ws .>> skipString "]" .>> ws) .>> skipString "in" .>> ws .>>. pexp |>> Exp.Let
let letStarExp = skipString "let*" >>. ws >>. many1 (skipString "[" >>. ws >>. pvar .>> ws .>> skipString "=" .>> ws .>>. pexp .>> ws .>> skipString "]" .>> ws) .>> skipString "in" .>> ws .>>. pexp |>> Exp.LetStar
let unpackExp = skipString "unpack" >>. ws >>. skipString "[" >>. ws >>. many1 (pvar .>> ws) .>> skipString "=" .>> ws .>>. pexp .>> ws .>> skipString "]" .>> ws .>> skipString "in" .>> ws .>>. pexp |>> (fun ((vars, exp1), exp2) -> Exp.Unpack(vars, exp1, exp2))
let letMutableExp = skipString "letmutable" >>. ws >>. many1 (skipString "[" >>. ws >>. pvar .>> ws .>> skipString "=" .>> ws .>>. pexp .>> ws .>> skipString "]" .>> ws) .>> skipString "in" .>> ws .>>. pexp |>> Exp.LetMutable
let setDynamicExp = skipString "setdynamic" >>. ws >>. pvar .>> ws .>> skipString "=" .>> ws .>>. pexp .>> ws .>> skipString "during" .>> ws .>>. pexp |>> (fun ((var, exp1), exp2) -> Exp.SetDynamic(var, exp1, exp2))
let letTypeExp =
    choice [
        letMutableExp
        letStarExp
        letExp
        unpackExp
        setDynamicExp
    ]

let ifExp = skipString "if" >>. ws >>. pexp .>> ws .>> skipString "then" .>> ws .>>. pexp .>> ws .>> skipString "else" .>> ws .>>. pexp |>> (fun ((exp1, exp2), exp3) -> Exp.If(exp1, exp2, exp3))
let condExp = skipString "cond" >>. ws >>. many1 (skipString "(" >>. ws >>. pexp .>> ws .>> skipString "==>" .>> ws .>>. pexp .>> skipString ")" .>> ws) .>> skipString "end" |>> Exp.Cond
let orExp = skipString "or(" >>. ws >>. sepBy pexp (skipString "," .>> ws) .>> skipString ")" |>> Exp.Or
let conditionalExp = 
    choice [
        ifExp
        condExp
        orExp
    ]

let isZeroExp : Parser<Exp, unit> = skipString "zero?(" >>. ws >>. pexp .>> skipString ")" |>> Exp.IsZero
let isOneExp : Parser<Exp, unit> = skipString "one?(" >>. ws >>. pexp .>> skipString ")" |>> Exp.IsOne
let minusExp = between (skipString "minus(" .>> ws) (skipString ")") pexp |>> Exp.Minus
let addExp = skipString "+(" >>. ws >>. pexp .>> ws .>> skipString "," .>> ws .>>. pexp .>> skipString ")" |>> Exp.Add
let diffExp = skipString "-(" >>. ws >>. pexp .>> ws .>> skipString "," .>> ws .>>. pexp .>> skipString ")" |>> Exp.Diff
let multExp = skipString "*(" >>. ws >>. pexp .>> ws .>> skipString "," .>> ws .>>. pexp .>> skipString ")" |>> Exp.Mult
let intDivExp = skipString "/(" >>. ws >>. pexp .>> ws .>> skipString "," .>> ws .>>. pexp .>> skipString ")" |>> Exp.IntDiv
let isEqualExp = skipString "=(" >>. ws >>. pexp .>> ws .>> skipString "," .>> ws .>>. pexp .>> skipString ")" |>> Exp.IsEqual
let isGreaterExp = skipString ">(" >>. ws >>. pexp .>> ws .>> skipString "," .>> ws .>>. pexp .>> skipString ")" |>> Exp.IsGreater
let isLessExp = skipString "<(" >>. ws >>. pexp .>> ws .>> skipString "," .>> ws .>>. pexp .>> skipString ")" |>> Exp.IsLess
let mathOpExp =
    choice [
        isZeroExp
        isOneExp
        minusExp
        addExp
        diffExp
        multExp
        intDivExp
        isEqualExp
        isGreaterExp
        isLessExp
    ]

let consExp = skipString "cons(" >>. ws >>. pexp .>> ws .>> skipString "," .>> ws .>>. pexp .>> ws .>> skipString ")" |>> Exp.Cons
let carExp = skipString "car(" >>. ws >>. pexp .>> skipString ")" |>> Exp.Car
let cdrExp = skipString "cdr(" >>. ws >>. pexp .>> skipString ")" |>> Exp.Cdr
let isNullExp = skipString "null?(" >>. ws >>. pexp .>> skipString ")" |>> Exp.IsNull
let emptyListExp : Parser<Exp, unit> = strWs "emptylist" |>> (fun _ -> Exp.EmptyList)
let listExp = skipString "list(" >>. ws >>. sepBy pexp (skipString "," .>> ws) .>> skipString ")" |>> Exp.List
let listOpExp =
    choice [
        consExp
        carExp
        cdrExp
        isNullExp
        emptyListExp
        listExp
    ]

let procExp = skipString "proc" >>. ws >>. (skipString "(" >>. ws >>. sepBy pvar (skipString "," .>> ws) .>> ws .>> skipString ")" .>> ws) .>>. pexp |>> Exp.Proc
let callExp = skipString "(" >>. ws >>. pexp .>> ws .>>. many1 (pexp .>> ws) .>> ws .>> skipString ")" |>> Exp.Call
let letProcExp = skipString "letproc" >>. ws >>. pvar .>> ws .>> skipString "(" .>> ws .>>. sepBy pvar (skipString "," .>> ws) .>> ws .>> skipString ")" .>> ws .>> skipString "=" .>> ws .>>. pexp .>> ws .>> skipString "in" .>> ws .>>. pexp |>> (fun (((var1, vars), exp1), exp2) -> Exp.LetProc(var1, vars, exp1, exp2))
let letRecExp = skipString "letrec" >>. ws >>. many1 (skipString "[" >>. ws >>. pvar .>> ws .>> skipString "(" .>> ws .>>. sepBy pvar (skipString "," .>> ws) .>> ws .>> skipString ")" .>> ws .>> skipString "=" .>> ws .>>. pexp .>> ws .>> skipString "]" .>> ws) .>> skipString "in" .>> ws .>>. pexp |>> (fun (procs, exp) -> 
                    let procs2 = procs |> List.map (fun ((var, vars), exp) -> (var, vars, exp))
                    Exp.LetRec(procs2, exp))
let lazyExp = skipString "lazy" >>. ws >>. pexp |>> Exp.Lazy
let procedureExp =
    choice [
        procExp
        callExp
        letProcExp
        letRecExp
        lazyExp
    ]

//let newRefExp = skipString "newref(" >>. ws >>. pexp .>> ws .>> skipString ")" |>> Exp.NewRef
let deRefExp = skipString "deref(" >>. ws >>. pexp .>> skipString ")" |>> Exp.DeRef
let setRefExp = skipString "setref(" >>. ws >>. pexp .>> ws .>> skipString "," .>> ws .>>. pexp .>> skipString ")" |>> Exp.SetRef
let refExp = skipString "ref" >>. ws >>. pvar |>> Exp.Ref
let setExp = skipString "set" >>. ws >>. pvar .>> ws .>> skipString "=" .>> ws .>>. pexp |>> Exp.Assign
let referenceExp =
    choice [
        //newRefExp
        refExp
        deRefExp
        setRefExp
        setExp
    ]

let beginExp = skipString "begin" >>. ws >>. sepBy1 pexp (skipString ";" .>> ws) .>> ws .>> skipString "end" |>> Exp.Begin

let pairExp = skipString "pair(" >>. ws >>. pexp .>> ws .>> skipString "," .>> ws .>>. pexp .>> ws .>> skipString ")" |>> Exp.NewPair
let leftExp = skipString "left(" >>. ws >>. pexp .>> skipString ")" |>> Exp.Left
let rightExp = skipString "right(" >>. ws >>. pexp .>> skipString ")" |>> Exp.Right
let setLeftExp = skipString "setleft(" >>. ws >>. pexp .>> ws .>> skipString "," .>> ws .>>. pexp .>> skipString ")" |>> Exp.SetLeft
let setRightExp = skipString "setright(" >>. ws >>. pexp .>> ws .>> skipString "," .>> ws .>>. pexp .>> skipString ")" |>> Exp.SetRight
let mutablePairExp =
    choice [
        pairExp
        leftExp
        rightExp
        setLeftExp
        setRightExp
    ]

let newArrayExp = skipString "newarray(" >>. ws >>. pexp .>> ws .>> skipString "," .>> ws .>>. pexp .>> ws .>> skipString ")" |>> Exp.NewArray
let arrayRefExp = skipString "arrayref(" >>. ws >>. pexp .>> ws .>> skipString "," .>> ws .>>. pexp .>> ws .>> skipString ")" |>> Exp.ArrayRef
let arraySetExp = skipString "arrayset(" >>. ws >>. pexp .>> ws .>> skipString "," .>> ws .>>. pexp .>> ws .>> skipString "," .>> ws .>>. pexp .>> ws .>> skipString ")" |>> (fun ((exp1, exp2), exp3) -> Exp.ArraySet(exp1, exp2, exp3))
let arrayLengthExp = skipString "arraylength(" >>. ws >>. pexp .>> ws .>> skipString ")" |>> Exp.ArrayLength
let arrayExp =
    choice [
        newArrayExp
        arrayRefExp
        arraySetExp
        arrayLengthExp
    ]

let newExp = skipString "new" >>. ws >>. pvar .>> ws .>> skipString "(" .>> ws .>>. sepBy pexp (skipString "," .>> ws) .>> ws .>> skipString ")" |>>  Exp.New
let sendExp = skipString "send" >>. ws >>. pexp .>> ws .>>. pvar .>> ws .>> skipString "(" .>> ws .>>. sepBy pexp (skipString "," .>> ws) .>> ws .>> skipString ")" |>> (fun ((exp1, var), exps) -> Exp.Send(exp1, var, exps))
let superExp = skipString "super" >>. ws >>. pvar .>> ws .>> skipString "(" .>> ws .>>. sepBy pexp (skipString "," .>> ws) .>> ws .>> skipString ")" |>> Exp.Super
let selfExp : Parser<Exp, unit> = skipString "self" |>> (fun _ -> Exp.Self)
let instanceOfExp = skipString "instanceof" >>. ws >>. pexp .>> ws .>>. pvar |>> Exp.InstanceOf
let fieldRefExp = skipString "fieldref" >>. ws >>. pexp .>> ws .>>. pvar |>> Exp.FieldRef
let fieldSetExp = skipString "fieldset" >>. ws >>. pexp .>> ws .>>. pvar .>> ws .>> skipString "=" .>> ws .>>. pexp |>> (fun ((exp1, var), exp2) -> Exp.FieldSet(exp1, var, exp2))
let objectExp =
    choice [
        newExp
        sendExp
        superExp
        selfExp
        instanceOfExp
        fieldRefExp
        fieldSetExp
    ]

let finalModifier = skipString "final" |>> (fun _ -> Final.Final)

let privateAccess = skipString "private" |>> (fun _ -> Access.Private)
let protectedAccess = skipString "protected" |>> (fun _ -> Access.Protected)
let publicAccess = skipString "public" |>> (fun _ -> Access.Public)
let access =
    choice [
        privateAccess
        protectedAccess
        publicAccess
    ]

let methodDecl = ((opt finalModifier) .>> ws .>>. access .>> ws .>> skipString "method" .>> ws .>>. pvar .>> ws .>> skipString "(" .>> ws .>>. sepBy pvar (skipString "," .>> ws) .>> ws .>> skipString ")" .>> ws .>>. pexp) |>> (fun ((((final, access), var), vars), exp) -> MethodDecl.MethodDecl(final, access, var, vars, exp))

let classDecl = 
    skipString "class" >>. ws >>. pvar .>> ws .>> skipString "extends" .>> ws .>>. pvar .>> ws .>>. 
    many (attempt (access .>> ws .>> skipString "field" .>> ws .>>. pvar .>> ws)) .>>. 
    many (methodDecl .>> ws) |>> 
    (fun (((className, superClassName), fields), methods) -> 
        ClassDecl.ClassDecl(className, superClassName, fields, methods))


let pprogram : Parser<Program, unit> = ws >>. many classDecl .>> ws .>>. pexp |>> Program.A
do pexpRef.Value <- 
    choice [
        conditionalExp
        procedureExp
        letTypeExp
        mutablePairExp
        referenceExp
        mathOpExp
        listOpExp
        beginExp
        arrayExp
        objectExp
        constExp
        varExp
    ]

let parseProgram (programText: string) =
    match run pprogram programText with
    | Success(result, _, _) -> Program.valueOfProgram result
    | Failure(errorMsg, _, _) -> failwith errorMsg

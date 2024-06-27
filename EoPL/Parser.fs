module Parser

open FParsec
open LetLang

let str = pstring
let ws = spaces
let strWs s = str s >>. ws
let skipStrWs s = skipString s >>. ws
let pipe6 p1 p2 p3 p4 p5 p6 f = pipe4 p1 p2 p3 (tuple3 p4 p5 p6) (fun a b c (d, e, g) -> f a b c d e g)

let pinteger : Parser<int, unit> = pint32 |>> int
let constExp : Parser<Exp, unit> = pinteger |>> Exp.Const
let pvar : Parser<Var, unit> = many1Satisfy isLetter |>> string
let varExp : Parser<Exp, unit> = pvar |>> Exp.Var

let pexp, pexpRef = createParserForwardedToRef<Exp, unit>()

let letExp = skipStrWs "let" >>. many1 (skipStrWs "[" >>. pvar .>> ws .>> skipStrWs "=" .>>. pexp .>> ws .>> skipStrWs "]") .>> ws .>> skipStrWs "in" .>>. pexp |>> Exp.Let
let letStarExp = skipStrWs "let*" >>. many1 (skipStrWs "[" >>. pvar .>> ws .>> skipStrWs "=" .>>. pexp .>> ws .>> skipStrWs "]") .>> ws .>> skipStrWs "in" .>>. pexp |>> Exp.LetStar
let unpackExp = skipStrWs "unpack" >>. skipStrWs "[" >>. many1 pvar .>> ws .>> skipStrWs "=" .>>. pexp .>> ws .>> skipStrWs "]" .>> ws .>> skipStrWs "in" .>>. pexp |>> (fun ((vars, exp1), exp2) -> Exp.Unpack(vars, exp1, exp2))
let letTypeExp =
    choice [
        letStarExp
        letExp
        unpackExp
    ]

let ifExp = skipStrWs "if" >>. pexp .>> ws .>> skipStrWs "then" .>>. pexp .>> ws .>> skipStrWs "else" .>>. pexp |>> (fun ((exp1, exp2), exp3) -> Exp.If(exp1, exp2, exp3))
let condExp = skipStrWs "cond" >>. many1 (skipStrWs "(" >>. pexp .>> skipStrWs "==>" .>>. pexp .>> skipStrWs ")") .>> skipStrWs "end" |>> Exp.Cond
let conditionalExp = 
    choice [
        ifExp
        condExp
    ]

let isZeroExp : Parser<Exp, unit> = skipStrWs "zero?(" >>. pexp .>> skipStrWs ")" |>> Exp.IsZero
let minusExp = between (skipStrWs "minus(") (skipStrWs ")") pexp |>> Exp.Minus
let addExp = skipStrWs "+(" >>. pexp .>> ws .>> skipStrWs "," .>>. pexp .>> skipStrWs ")" |>> (fun (exp1, exp2) -> Exp.Add(exp1, exp2))
let diffExp = skipStrWs "-(" >>. pexp .>> ws .>> skipStrWs "," .>>. pexp .>> skipStrWs ")" |>> (fun (exp1, exp2) -> Exp.Diff(exp1, exp2))
let multExp = skipStrWs "*(" >>. pexp .>> ws .>> skipStrWs "," .>>. pexp .>> skipStrWs ")" |>> (fun (exp1, exp2) -> Exp.Mult(exp1, exp2))
let intDivExp = skipStrWs "/(" >>. pexp .>> ws .>> skipStrWs "," .>>. pexp .>> skipStrWs ")" |>> (fun (exp1, exp2) -> Exp.IntDiv(exp1, exp2))
let isEqualExp = skipStrWs "=(" >>. pexp .>> ws .>> skipStrWs "," .>>. pexp .>> skipStrWs ")" |>> (fun (exp1, exp2) -> Exp.IsEqual(exp1, exp2))
let isGreaterExp = skipStrWs ">(" >>. pexp .>> ws .>> skipStrWs "," .>>. pexp .>> skipStrWs ")" |>> (fun (exp1, exp2) -> Exp.IsGreater(exp1, exp2))
let isLessExp = skipStrWs "<(" >>. pexp .>> ws .>> skipStrWs "," .>>. pexp .>> skipStrWs ")" |>> (fun (exp1, exp2) -> Exp.IsLess(exp1, exp2))
let mathOpExp =
    choice [
        isZeroExp
        minusExp
        addExp
        diffExp
        multExp
        intDivExp
        isEqualExp
        isGreaterExp
        isLessExp
    ]

let consExp = skipStrWs "cons(" >>. pexp .>> ws .>> skipStrWs "," .>>. pexp .>> ws .>> skipStrWs ")" |>> (fun (exp1, exp2) -> Exp.Cons(exp1, exp2))
let carExp = skipStrWs "car(" >>. pexp .>> skipStrWs ")" |>> Exp.Car
let cdrExp = skipStrWs "cdr(" >>. pexp .>> skipStrWs ")" |>> Exp.Cdr
let isNullExp = skipStrWs "null?(" >>. pexp .>> skipStrWs ")" |>> Exp.IsNull
let emptyListExp : Parser<Exp, unit> = strWs "emptylist" |>> (fun _ -> Exp.EmptyList)
let listExp = skipStrWs "list(" >>. sepBy pexp (skipStrWs ",") .>> skipStrWs ")" |>> Exp.List
let listOpExp =
    choice [
        consExp
        carExp
        cdrExp
        isNullExp
        emptyListExp
        listExp
    ]

let pprogram : Parser<Program, unit> = ws >>. pexp |>> Program.A
do pexpRef.Value <- 
    choice [
        conditionalExp
        letTypeExp
        mathOpExp
        listOpExp
        constExp
        varExp
    ]

let parseProgram (programText: string) =
    match run pprogram programText with
    | Success(result, _, _) -> Program.valueOfProgram result
    | Failure(errorMsg, _, _) -> failwith errorMsg

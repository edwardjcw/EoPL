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
let pvar : Parser<Var, unit> = many1Satisfy isLetter |>> string
let varExp : Parser<Exp, unit> = pvar |>> Exp.Var

let pexp, pexpRef = createParserForwardedToRef<Exp, unit>()

let letExp = skipString "let" >>. ws >>. many1 (skipString "[" >>. ws >>. pvar .>> ws .>> skipString "=" .>> ws .>>. pexp .>> ws .>> skipString "]" .>> ws) .>> skipString "in" .>> ws .>>. pexp |>> Exp.Let
let letStarExp = skipString "let*" >>. ws >>. many1 (skipString "[" >>. ws >>. pvar .>> ws .>> skipString "=" .>> ws .>>. pexp .>> ws .>> skipString "]" .>> ws) .>> skipString "in" .>> ws .>>. pexp |>> Exp.LetStar
let unpackExp = skipString "unpack" >>. ws >>. skipString "[" >>. ws >>. many1 (pvar .>> ws) .>> skipString "=" .>> ws .>>. pexp .>> ws .>> skipString "]" .>> ws .>> skipString "in" .>> ws .>>. pexp |>> (fun ((vars, exp1), exp2) -> Exp.Unpack(vars, exp1, exp2))
let letTypeExp =
    choice [
        letStarExp
        letExp
        unpackExp
    ]

let ifExp = skipString "if" >>. ws >>. pexp .>> ws .>> skipString "then" .>> ws .>>. pexp .>> ws .>> skipString "else" .>> ws .>>. pexp |>> (fun ((exp1, exp2), exp3) -> Exp.If(exp1, exp2, exp3))
let condExp = skipString "cond" >>. ws >>. many1 (skipString "(" >>. ws >>. pexp .>> ws .>> skipString "==>" .>> ws .>>. pexp .>> skipString ")" .>> ws) .>> skipString "end" |>> Exp.Cond
let conditionalExp = 
    choice [
        ifExp
        condExp
    ]

let isZeroExp : Parser<Exp, unit> = skipString "zero?(" >>. ws >>. pexp .>> skipString ")" |>> Exp.IsZero
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

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
let isZeroExp : Parser<Exp, unit> = skipStrWs "zero?" >>. pexp |>> Exp.IsZero
let letExp = skipStrWs "let" >>. pvar .>> ws .>> skipStrWs "=" .>>. pexp .>> ws .>> skipStrWs "in" .>>. pexp |>> (fun ((var, exp1), body) -> Exp.Let(var, exp1, body))
let diffExp = skipStrWs "-(" >>. pexp .>> ws .>> skipStrWs "," .>>. pexp .>> skipStrWs ")" |>> (fun (exp1, exp2) -> Exp.Diff(exp1, exp2))
let ifExp = skipStrWs "if" >>. pexp .>> ws .>> skipStrWs "then" .>>. pexp .>> ws .>> skipStrWs "else" .>>. pexp |>> (fun ((exp1, exp2), exp3) -> Exp.If(exp1, exp2, exp3))
let pprogram : Parser<Program, unit> = ws >>. pexp |>> Program.A
do pexpRef.Value <- 
    choice [
        ifExp
        letExp
        diffExp
        isZeroExp 
        constExp
        varExp
    ]

let parseProgram (programText: string) =
    match run pprogram programText with
    | Success(result, _, _) -> Program.valueOfProgram result
    | Failure(errorMsg, _, _) -> failwith errorMsg

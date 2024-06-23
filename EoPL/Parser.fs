module Parser

open FParsec
open LetLang

//type Parser<'Result, 'UserState> = CharStream<'UserState> -> Reply<'Result>

let str = pstring
let ws = spaces
let strWs s = str s >>. ws
let skipStrWs s = skipString s >>. ws
let pipe6 p1 p2 p3 p4 p5 p6 f = pipe4 p1 p2 p3 (tuple3 p4 p5 p6) (fun a b c (d, e, g) -> f a b c d e g)
let pinteger : Parser<int, unit> = pint32 |>> int
let constExp : Parser<Exp, unit> = pinteger |>> Exp.Const
let pvar : Parser<Var, unit> = many1Satisfy isLetter |>> string
let varExp : Parser<Exp, unit> = pvar |>> Exp.Var
let rec pexp () : Parser<Exp, unit> = 
    choice [isZeroExp; constExp; varExp]
    //isZeroExp <|> (*letExp <|> diffExp <|>*) constExp <|> varExp 
and isZeroExp : Parser<Exp, unit> = skipStrWs "zero?" >>. pexp () |>> Exp.IsZero
(*and letExp = pipe6 (strWs "let") (pvar) (strWs "=") (pexp()) (strWs "in") (pexp()) (fun _ var _ exp1 _ body -> Exp.Let(var, exp1, body))
and diffExp = pipe2 (strWs "-" >>. pexp ()) (strWs "," >>. pexp ()) (fun exp1 exp2 -> Exp.Diff(exp1, exp2))*)
and pprogram : Parser<Program, unit> = ws >>. pexp () |>> Program.A

and parseProgram (programText: string) =
    match run pprogram programText with
    | Success(result, _, _) -> Program.valueOfProgram result
    | Failure(errorMsg, _, _) -> failwith errorMsg

module Tests

open FsUnit
open Parser
open LetLang

let ``parse const`` () =
    let programText = "1"
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 1)

let ``parse var`` () =
    let programText = "let x = 1 in x"
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 1)

let ``parse diff`` () =
    let programText = "-(1, 2)"
    let program = parseProgram programText
    program |> should equal (ExpVal.Num -1)

let ``parse isZero`` () =
    let programText = "zero? 0"
    let program = parseProgram programText
    program |> should equal (ExpVal.Bool true)

let ``parse if`` () =
    let programText = "if zero? 0 then 1 else 2"
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 1)

let ``parse let`` () =
    let programText = "let x = 1 in let y = 2 in x"
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 1)

let ``parse minus`` () =
    let programText = "minus(1)"
    let program = parseProgram programText
    program |> should equal (ExpVal.Num -1)

let ``parse add`` () =
    let programText = "+(1, 2)"
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 3)

let ``parse mult`` () =
    let programText = "*(2, 3)"
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 6)

let ``parse intDiv`` () =
    let programText = "/(6, 3)"
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 2)

let ``parse isEqual`` () =
    let programText = "=(1, 1)"
    let program = parseProgram programText
    program |> should equal (ExpVal.Bool true)

let ``parse isGreater`` () =
    let programText = ">(2, 1)"
    let program = parseProgram programText
    program |> should equal (ExpVal.Bool true)

let ``parse isLess`` () =
    let programText = "<(1, 2)"
    let program = parseProgram programText
    program |> should equal (ExpVal.Bool true)

let ``parse cons`` () =
    let programText = "cons(1, emptylist)"
    let program = parseProgram programText
    program |> should equal (ExpVal.List (ValList.Cons (ExpVal.Num 1, ValList.Empty)))

let ``parse car`` () =
    let programText = "car(cons(1, emptylist))"
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 1)

let ``parse cdr`` () =
    let programText = "cdr(cons(1, emptylist))"
    let program = parseProgram programText
    program |> should equal (ExpVal.List ValList.Empty)

let ``parse isNull`` () =
    let programText = "null?(emptylist)"
    let program = parseProgram programText
    program |> should equal (ExpVal.Bool true)

let ``parse emptyList`` () =
    let programText = "emptylist"
    let program = parseProgram programText
    program |> should equal (ExpVal.List ValList.Empty)

let runTests () =
    let tests = 
        [ 
            ``parse const``
            ``parse var``
            ``parse diff``
            ``parse isZero``
            ``parse if``
            ``parse let``
            ``parse minus``
            ``parse add``
            ``parse mult``
            ``parse intDiv``
            ``parse isEqual``
            ``parse isGreater``
            ``parse isLess``
            ``parse cons``
            ``parse car``
            ``parse cdr``
            ``parse isNull``
            ``parse emptyList``
        ]
    tests |> List.iter (fun test -> test())



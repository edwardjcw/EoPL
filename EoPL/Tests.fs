module Tests

open FsUnit
open Parser
open LetLang

let ``parse const`` () =
    let programText = "1"
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 1)

let ``parse var`` () =
    let programText = "let [x = 1] in x"
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 1)

let ``parse diff`` () =
    let programText = "-(1, 2)"
    let program = parseProgram programText
    program |> should equal (ExpVal.Num -1)

let ``parse isZero`` () =
    let programText = "zero?(0)"
    let program = parseProgram programText
    program |> should equal (ExpVal.Bool true)

let ``parse isOne`` () =
    let programText = "one?(1)"
    let program = parseProgram programText
    program |> should equal (ExpVal.Bool true)

let ``parse if`` () =
    let programText = "if zero?(0) then 1 else 2"
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 1)

let ``parse cond`` () =
    let programText = "cond (zero?(0) ==> 1) end"
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 1)

let ``parse or`` () =
    let programText = "or(zero?(5), one?(2), zero?(0))"
    let program = parseProgram programText
    program |> should equal (ExpVal.Bool true)

let ``parse let`` () =
    let programText = "let [x = 30] in let [x = -(x,1)] [y = -(x,2)] in -(x,y)"
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 1)

let ``parse let*`` () =
    let programText = "let* [x = 30] [y = -(x,1)] in -(x,y)"
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 1)

let ``parse unpack`` () =
    let programText = "let [x = cons(1, cons(2, emptylist))] in unpack [a b = x] in -(a,b)"
    let program = parseProgram programText
    program |> should equal (ExpVal.Num -1)

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
    let programText = "cons(1, cons(2, emptylist))"
    let program = parseProgram programText
    program |> should equal (ExpVal.List [ExpVal.Num 1; ExpVal.Num 2])

let ``parse car`` () =
    let programText = "car(cons(1, emptylist))"
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 1)

let ``parse cdr`` () =
    let programText = "cdr(cons(1, emptylist))"
    let program = parseProgram programText
    program |> should equal (ExpVal.List [])

let ``parse isNull`` () =
    let programText = "null?(emptylist)"
    let program = parseProgram programText
    program |> should equal (ExpVal.Bool true)

let ``parse emptyList`` () =
    let programText = "emptylist"
    let program = parseProgram programText
    program |> should equal (ExpVal.List [])

let ``parse list`` () =
    let programText = "list(1, 2, 3)"
    let program = parseProgram programText
    program |> should equal (ExpVal.List [ExpVal.Num 1; ExpVal.Num 2; ExpVal.Num 3])

let ``parse procCall`` () =
    let programText = "(proc (x, y) list(x,y) 1 2)"
    let program = parseProgram programText
    program |> should equal (ExpVal.List [ExpVal.Num 1; ExpVal.Num 2])

let ``parse letproc`` () =
    let programText = "letproc f (x, y) = list(x,y) in (f 1 2)"
    let program = parseProgram programText
    program |> should equal (ExpVal.List [ExpVal.Num 1; ExpVal.Num 2])

let ``parse letrec1`` () =
    let programText = "letrec [f (x, y) = if zero?(x) then 0 else -((f -(x,1) y), minus(y))] in (f 3 4)"
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 12)

let ``parse letrec2`` () =
    let programText = 
        "
            letrec [even(x) = if zero?(x) then 1 else (odd -(x,1))]
                   [odd(x) = if zero?(x) then 0 else (even -(x,1))]
            in (odd 13)
        "
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 1)

//let ``parse newRef and deRef`` () =
//    let programText = "let [x = newref(1)] in deref(x)"
//    let program = parseProgram programText
//    program |> should equal (ExpVal.Num 1)

let ``parse begin`` () =
    let programText = "begin 1; 2; 3 end"
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 3)

//let ``parse setRef`` () =
//    let programText = "let [x = newref(1)] in begin setref(x, 2); deref(x) end"
//    let program = parseProgram programText
//    program |> should equal (ExpVal.Num 2)

let ``parse set`` () =
    let programText = "letmutable [x = 1] in begin set x = 2; x end"
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 2)

let ``parse setdynamic`` () =
    let programText = 
        "
            letmutable [x = 11]
            in let [p = proc (y) -(y, x)]
               in -(setdynamic x = 17 during (p 22), (p 13))
        "
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 3)

let ``parse setleft and left`` () =
    let programText = "let [x = pair(1, 2)] in begin setleft(x, 3); left(x) end"
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 3)

let ``parse setright and right`` () =
    let programText = "let [x = pair(1, 2)] in begin setright(x, 3); right(x) end"
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 3)

let runTests () =
    let tests = 
        [ 
            ``parse const``
            ``parse var``
            ``parse diff``
            ``parse isZero``
            ``parse isOne``
            ``parse if``
            ``parse cond``
            ``parse or``
            ``parse let``
            ``parse let*``
            ``parse unpack``
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
            ``parse list``
            ``parse procCall``
            ``parse letproc``
            ``parse letrec1``
            ``parse letrec2``
            //``parse newRef and deRef``
            ``parse begin``
            ``parse set``
            ``parse setdynamic``
            ``parse setleft and left``
            ``parse setright and right``
        ]
    tests |> List.iter (fun test -> test())



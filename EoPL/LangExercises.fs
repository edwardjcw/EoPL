module LangExercises

open FsUnit
open Parser
open LetLang

let ``parse Exercise 3.23`` () =
    let programText = 
        "
        let [makemult = proc (maker)
                         proc (x)
                          proc (y)
                           if zero?(x)
                           then 0
                           else -((((maker maker) -(x,1)) y), minus(y))]
         in let [times = proc (x) proc (y) (((makemult makemult) x) y)]
            in let [makefact = proc (maker)
                                proc (n)
                                 if zero?(n)
                                 then 1
                                 else ((times n) ((maker maker) -(n,1)))]
                in let [factorial = proc (n) ((makefact makefact) n)]
                   in (factorial 4) 
        "
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 24)

let runExercises () =
    let exercises = 
        [ 
            ``parse Exercise 3.23`` 
        ]
    exercises |> List.iter (fun test -> test())
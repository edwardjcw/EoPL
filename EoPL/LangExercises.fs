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

let ``parse Exercise 3.24 even`` () =
    let programText = 
        "
        let [makeEven = proc (maker)
                         proc (f)
                          proc (x)
                           if zero?(x)
                           then 1
                           else (((f f) maker) -(x,1))]
        in let [makeOdd = proc (maker)
                           proc (f)
                            proc (x)
                             if zero?(x)
                             then 0
                             else (((f f) maker) -(x,1))]
            in let [odd = proc (x) (((makeOdd makeOdd) makeEven) x)]
               in let [even = proc (x) (((makeEven makeEven) makeOdd) x)]
                  in (even 12)
        "
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 1)

let ``parse Exercise 3.24 odd`` () =
    let programText = 
        "
        let [makeEven = proc (maker)
                         proc (f)
                          proc (x)
                           if zero?(x)
                           then 1
                           else (((f f) maker) -(x,1))]
        in let [makeOdd = proc (maker)
                           proc (f)
                            proc (x)
                             if zero?(x)
                             then 0
                             else (((f f) maker) -(x,1))]
            in let [odd = proc (x) (((makeOdd makeOdd) makeEven) x)]
               in let [even = proc (x) (((makeEven makeEven) makeOdd) x)]
                  in (odd 13)
        "
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 1)

let ``parse Exercise 3.24 not even`` () =
    let programText = 
        "
        let [makeEven = proc (maker)
                         proc (f)
                          proc (x)
                           if zero?(x)
                           then 1
                           else (((f f) maker) -(x,1))]
        in let [makeOdd = proc (maker)
                           proc (f)
                            proc (x)
                             if zero?(x)
                             then 0
                             else (((f f) maker) -(x,1))]
            in let [odd = proc (x) (((makeOdd makeOdd) makeEven) x)]
               in let [even = proc (x) (((makeEven makeEven) makeOdd) x)]
                  in (even 13)
        "
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 0)

let ``parse Exercise 4.39`` () =
    let programText = 
        "
            letproc f (x) = begin 
                             -(x,2);
                             -(x,2)
                            end
            in let [g = letmutable [counter = minus(1)]
                        in proc(x) begin
                                    set counter = -(counter,minus(1));
                                    -(x,counter)
                                   end]
            in (f lazy (g 3))
        "
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 1)

let ``parse Exercise 9.1(1)`` () =
    let programText = 
        "
            class queue extends object
                field q
                method initialize () set q = emptylist
                method empty () null?(q)
                method enqueue (x) set q = cons(x,q)
                method dequeue ()
                   letmutable [value = 0]
                   in letrec [f (x) = if null?(cdr(x)) then
                                        begin 
                                         set value = car(x);
                                         emptylist
                                        end
                                      else cons(car(x), (f cdr(x)))]
                      in let [q2 = (f q)]
                         in begin
                             set q = q2;
                             value
                            end
            let [obj = new queue()]
            in begin
                 send obj enqueue(1);
                 send obj enqueue(2);
                 send obj enqueue(3);
                 send obj enqueue(4);
                 send obj dequeue();
                 send obj dequeue()
               end
        "
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 2)

let runExercises () =
    let exercises = 
        [ 
            ``parse Exercise 3.23``
            ``parse Exercise 3.24 even``
            ``parse Exercise 3.24 odd``
            ``parse Exercise 3.24 not even``
            ``parse Exercise 4.39``
            ``parse Exercise 9.1(1)``
        ]
    exercises |> List.iter (fun test -> test())
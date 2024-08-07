module Tests

open FsUnit
open Parser
open Lang

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

let ``parse newarray, arrayset, arrayref`` () =
    let programText = "let [x = newarray(3, 0)] in begin arrayset(x, 1, 1); arrayref(x, 1) end"
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 1)

let ``parse arraylength`` () =
    let programText = "let [x = newarray(3, 0)] in arraylength(x)"
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 3)

let ``parse ref, deref, setref`` () =
    let programText = 
        "
            letmutable [a = 3]
            in letmutable [b = 4]
               in let [swap = proc (x) proc (y)
                               let [temp = deref(x)]
                               in begin
                                   setref(x,deref(y));
                                   setref(y,temp)
                                  end]
                  in begin
                      ((swap ref a) ref b);
                      -(a,b)
                     end
        "
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 1)

let ``parse lazy`` () =
    let programText = 
        "
            letproc f (x) = -(x,2)
            in letproc g (x) = -(x,1)
               in (f lazy (g 3))
        "
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 0)

let ``parse class self`` () =
    let programText = 
        "
            class c1 extends object
                public method initialize () 1
                public method test1 () send self test2()
                public method test2 () 13
            let [o1 = new c1()]
            in send o1 test1()
        "
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 13)

let ``parse class method parameters`` () =
    let programText = 
        "
            class c1 extends object
                public field x
                public field y
                public method initialize (initx, inity)
                    begin
                     set x = initx;
                     set y = inity
                    end
                public method move (dx, dy)
                    begin
                     set x = +(x,dx);
                     set y = +(y,dy)
                    end
                public method getLocation () list(x,y)
            let [o1 = new c1(3,4)]
            in begin 
                send o1 move(1,1);
                send o1 getLocation()
               end
        "
    let program = parseProgram programText
    program |> should equal (ExpVal.List [ExpVal.Num 4; ExpVal.Num 5])

let ``parse class inheritance1`` () =
    let programText = 
        "
            class c1 extends object
                public field x
                public method initialize (initx)
                    set x = initx
                public method getX () x
            class c2 extends c1
                public field y
                public method initialize (initx, inity)
                    begin
                     super initialize(initx);
                     set y = inity
                    end
                public method getY () y
            let [o2 = new c2(3,4)]
            in list(send o2 getX(), send o2 getY())
        "
    let program = parseProgram programText
    program |> should equal (ExpVal.List [ExpVal.Num 3; ExpVal.Num 4])

let ``parse class inheritance2`` () =
    let programText = 
        "
            class c1 extends object
                public method initialize () 1
                public method m1 () send self m2()
                public method m2 () 13
            class c2 extends c1
                public method m1 () 22
                public method m2 () 23
                public method m3 () super m1()
            class c3 extends c2
                public method m1 () 32
                public method m2 () 33
            let [o3 = new c3()]
            in send o3 m3()
        "
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 33)

let ``parse instanceof`` () =
    let programText = 
        "
            class c1 extends object
                public method initialize () 1
            class c2 extends c1
            class c3 extends c2
            class c4 extends object
            let [o3 = new c3()]
            in list(instanceof o3 c3,
                    instanceof o3 c2,
                    instanceof o3 c1,
                    instanceof o3 object,
                    instanceof o3 c4)
        "
    let program = parseProgram programText
    program |> should equal (ExpVal.List [ExpVal.Bool true; ExpVal.Bool true; ExpVal.Bool true; ExpVal.Bool true; ExpVal.Bool false])

let ``parse fieldref and fieldset`` () =
    let programText = 
        "
            class c1 extends object
                public field x
                public method initialize () set x = 5
            class c2 extends c1
                public field y
                public method initialize ()
                   begin 
                      super initialize();
                      set y = 6
                   end
            let [o2 = new c2()]
            in begin
                fieldset o2 x = 10;
                fieldset o2 y = 11;
                list(fieldref o2 x, fieldref o2 y)
               end
        "
    let program = parseProgram programText
    program |> should equal (ExpVal.List [ExpVal.Num 10; ExpVal.Num 11])

let ``parse private method`` () =
    let programText = 
        "
            class c1 extends object
                public method initialize () 5
                public method test1 () send self test2()
                private method test2 () 13
            let [o1 = new c1()]
            in send o1 test1()
        "
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 13)

let ``parse protected method`` () =
    let programText = 
        "
            class c1 extends object
                public method initialize () 5
            class c2 extends c1
                protected method test3 () 12
            class c3 extends c2
                public method test4 () super test3()
            let [o3 = new c3()]
            in send o3 test4()
        "
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 12)

let ``parse private field`` () =
    let programText = 
        "
            class c1 extends object
                private field x
                public method initialize () set x = 5
                public method getX () x
            let [o1 = new c1()]
            in send o1 getX()
        "
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 5)

let ``parse protected field`` () =
    let programText = 
        "
            class c1 extends object
                protected field x
                public method initialize () set x = 5
            class c2 extends c1
                public method getX () x
            let [o2 = new c2()]
            in send o2 getX()
        "
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 5)

let ``parse final method`` () =
    let programText = 
        "
            class c1 extends object
                protected field x
                public method initialize () set x = 5
            class c2 extends c1
                final public method getX () x
            class c3 extends c2
                protected field y
                public method setY (yy) set y = yy
                public method getY() y
            let [o3 = new c3()]
            in begin
                send o3 setY(10);
                list(send o3 getX(), send o3 getY())
               end
        "
    let program = parseProgram programText
    program |> should equal (ExpVal.List [ExpVal.Num 5; ExpVal.Num 10])

let ``parse static field`` () =
    let programText = 
        "
            class c1 extends object
                static r = 10
                static s = -(r,10)
                private field k
                public method initialize () 
                   begin 
                      set k = 1;
                      set s = +(s, k)
                   end
                public method getS () s
            let [o1 = new c1()]
                [o2 = new c1()]
                [o3 = new c1()]
            in send o1 getS()
        "
    let program = parseProgram programText
    program |> should equal (ExpVal.Num 3)

let ``parse overload method`` () =
    let programText = 
        "
            class c1 extends object
                protected field x
                public method initialize () set x = 5
                public method initialize (y) set x = y
                public method getX () x
                public method addToX () +(x,1)
                public method addToX (y) +(x,+(y,1))
            class c2 extends c1
                public method addToX (y) +(x,y)
            let [o1 = new c1()]
                [o2 = new c2(10)]
            in list(send o1 getX(), 
                    send o2 getX(),
                    send o1 addToX(),
                    send o2 addToX(2))
        "
    let program = parseProgram programText
    program |> should equal (ExpVal.List [ExpVal.Num 5; ExpVal.Num 10; ExpVal.Num 6; ExpVal.Num 12])

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
            ``parse newarray, arrayset, arrayref``
            ``parse arraylength``
            ``parse ref, deref, setref``
            ``parse lazy``
            ``parse class self``
            ``parse class method parameters``
            ``parse class inheritance1``
            ``parse class inheritance2``
            ``parse instanceof``
            ``parse fieldref and fieldset``
            ``parse private method``
            ``parse protected method``
            ``parse private field``
            ``parse protected field``
            ``parse final method``
            ``parse static field``
            ``parse overload method``
        ]
    tests |> List.iter (fun test -> test())



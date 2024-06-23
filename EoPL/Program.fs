open Parser
open LetLang

// get text from console
let programText = System.Console.ReadLine()

// parse the program
let program = parseProgram programText

// print the result
printfn "%A" program

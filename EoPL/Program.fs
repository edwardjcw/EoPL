open Parser
open LetLang

// get text from console
// read until ;; is entered
let rec readProgramText (acc: string) =
    let line = System.Console.ReadLine()
    if line = ";;" then acc else readProgramText (acc + line + "\n")
let programText = readProgramText ""

// parse the program
let program = parseProgram programText

// print the result
printfn "%A" program

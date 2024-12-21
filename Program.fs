open Calculator
open NumberGuesser
open StudentGradeBook
open TansformPipeline1

let projectOptions = [|"1) Calculator";
                        "2) Number Guesser";
                        "3) Student grade book";
                        "4) Transform pipeline";
                        "5) Recursive sequence generaror"
                        |]

[<EntryPoint>]
let main argz =
    
    printfn "This is a demo App showing my learning process in F#\nChoose from one of the following"
    Array.iter (fun x -> printfn $"  - {x}") projectOptions
    
    let chosen = System.Console.ReadLine()

    match chosen with
    | "1" -> Calculator.calculateEquation [|"1"; "+"; "3"|]
    | "2" -> NumberGuesser.numberGuesser [|"10"; "20"|]
    | "3" -> StudentGradeBook.RunStudentTest () |> ignore
    | "4" -> TansformPipeline1.main () |> ignore
    | "5" -> RecursiveSeqGen.seqGen()|> ignore
    | _ -> printf "Invalid option"

    
    
    0
module Calculator


let add x y = x + y
let subtract x y = x - y
let multiply x y = x * y
let divide x y = if y = 0 then None else x / y |> Some

let calculate x op y =
    match op with 
    | "+" -> add x y |> Some
    | "-" -> subtract x y |> Some
    | "*" -> multiply x y |> Some
    | "/" -> divide x y
    | _ -> None



let calculateEquation (argv: string[]) =
    
    if argv.Length = 3 then
        let n1Parsed, n1 = System.Int32.TryParse(argv.[0])
        let n2Parsed, n2 = System.Int32.TryParse(argv.[2])
        let op = argv.[1]

        match (n1Parsed, n2Parsed) with 
        | (true, true) ->
            match calculate n1 op n2 with
            | Some result -> 
                printfn "Result = %d" result
            | _ -> printfn "Invalid operator or division by zero."
        | _ -> printfn "Invalid number inputs. Please provide valid integers."
    
    else
        printfn "Please provide exactly 3 arguments: <operator> <num1> <num2>"
    
    
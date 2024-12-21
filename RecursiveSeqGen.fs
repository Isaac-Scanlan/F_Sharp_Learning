module RecursiveSeqGen

let rec fibonacci (x: int) = 
    if x <= 1 then x else fibonacci (x - 1) +  fibonacci (x - 2)

let lessCallsFib (x: int): int =

    let rec fibonacci2 (x: int): int list = 

        let list = if x <= 1 then [0;0] else (fibonacci2 (x - 1))
        let value = List.reduce (+) list
        match value with
        | 0 -> 1 :: (list |> List.take (List.length list - 1))
        | _ -> value :: (list |> List.take (List.length list - 1))

    (fibonacci2 x).[0]

let fibonacciMemoised x = 
    let cache = Array.create(x + 1) 0
    if x > 0 then cache.[1] <- 1
    for i in 2..x do
        cache.[i] <- cache.[i - 1] +  cache.[i - 2]

    cache.[x]
    
let fibonacciTail x =
    let rec loop a b n =
        if n = 0 then a
        else loop b (a + b) (n - 1)

    loop 0 1 x

let rec factorial (x: int) =
    if x = 0 then 1 else x * factorial (x - 1)

let factorialTail x =
    let rec loop acc x =
        if x = 0 then acc
        else loop (acc * x) (x - 1)
    loop 1 x
        
let arithmeticProgression start commonDiff = 
    Seq.init 10 (fun x -> x + commonDiff)

let geometricProgression start commonRatio = 
    Seq.init 10 (fun x -> x * commonRatio)
    

let seqGen argz =
    printfn "fibonacci val = %d" (fibonacci 10)
    printfn "lessCallsFib val = %d" (lessCallsFib 10)
    printfn "fibonacciMemoised val = %d" (fibonacciMemoised 10)
    printfn "fibonacciTail val = %d" (fibonacciTail 10)

    printfn "factorial val = %d" (factorial 10)
    printfn "factorialTail val = %d" (factorialTail 10)

    printfn "arithmeticProgression val = %A" (arithmeticProgression 10 3)
    printfn "geometricProgression val = %A" (geometricProgression 10 3)
    0
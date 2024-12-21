module TansformPipeline1

// Core Functions
let getParity (nums: float[]) (x: int) : float array =
    nums |> Array.filter (fun num -> (num % 2.0) - float x = 0.0)
    
let getEvenNums nums = getParity nums 0
let getOddNums nums = getParity nums 1

let add x y : float = x + y
let addToEach (x: float) (nums: float[]) : float[] =
    nums |> Array.map (fun num -> add num x)

let subtract x y = x - y
let subtractFromEach x nums =
    nums |> Array.map (fun num -> subtract num x)

let multiply x y = x * y
let multEachBy (x: float) (nums: float[]) : float[] =
    nums |> Array.map (fun num -> multiply num x)

let divide x y = if y = 0.0 then None else Some (x / y)
let divideEachBy x nums =
    let foo = nums |> Array.map (fun num -> divide num x)
    foo |> Array.choose id

let powerInt (x: int) (n: int) = pown n x
let powerIntEach x nums =
    nums |> Array.map (fun num -> powerInt num x)

let powerFloat (x: float) (n: float) = x ** n
let powerFloatEach x nums =
    nums |> Array.map (fun num -> powerFloat num x)

let Sum (arry: float[]) =
    Array.reduce (add) arry

let Average list = 
    let sum = Sum list
    sum / float (list.Length)

let quadratic x a b c= 
        let axSquared = (fun xx aa -> multiply aa (powerFloat xx 2.0))
        let bx = multiply b x
        Sum [|axSquared x a; bx; c|]

let applyPipeline (pipeline: (float[] -> float[]) list) (nums: float[]) =
    pipeline |> List.fold (fun acc transform -> transform acc) nums

let applyPipeline2 (pipeline: (float[] -> float[]) list) (nums: float[]) =
    let composedFunction = pipeline |> List.reduce (>>) 
    composedFunction nums


let main argz =
    let nums = [|1.0; 2.0; 3.0; 4.0; 5.0|]

    let pipeline1 = [multEachBy 2.0; addToEach 1.0; subtractFromEach 2.0]
    let pipeline2 = [divideEachBy 2.0; powerFloatEach 2.0]
    let pipeline3 = pipeline1 @ [multEachBy 2.0] @ pipeline2
    
    let transformed1 = applyPipeline pipeline1 nums
    let transformed2 = applyPipeline pipeline2 nums
    let transformed3 = applyPipeline pipeline3 nums

    printfn "Original Numbers: %A" nums
    printfn "Transformed Pipeline 1: %A" transformed1
    printfn "Transformed Pipeline 2: %A" transformed2
    printfn "Transformed Pipeline 2: %A" transformed3

    let evenNums = getParity nums 0
    let oddNums = getParity nums 1
    printfn "Even Numbers: %A" evenNums
    printfn "Odd Numbers: %A" oddNums

    printfn "Sum of Original: %f" (Sum nums)
    printfn "Average of Original: %f" (Average nums)

    let quadResult = quadratic 5.0 1.0 1.0 0.0
    printfn "Quadratic Result for x=5, a=1, b=1, c=0: %f" quadResult

    0
module NumberGuesser

open System

let parseBounds (args: string[]) =
    match args with
    | [|_; lowerStr; higherStr|] -> 
        let lowerParsed, lower = Int32.TryParse(lowerStr)
        let upperParsed, upper = Int32.TryParse(higherStr)
        if lowerParsed && upperParsed then Some (lower, upper) else None
    | _ -> Some(10, 20)

let isEven num =
    if num % 2 = 0 then "even" else "odd"

let getOneMultiple (rand: System.Random) num = 
    let multiples = List.filter(fun x -> num % x = 0) [1..num/2]
    let length = multiples.Length

    if length > 1 then 
        let randMult = rand.Next(1, length)
        multiples.[randMult]
    else 
        -1

let getNumInfo (rand: System.Random) num =
    let mult = getOneMultiple rand num
    let clue = if mult = -1 then "s of 1 and itself" else $" of {mult}"
    ((isEven num), clue)

let numberGuesser argz = 
    
    let num = Random()
    let bounds = parseBounds argz

    let number = 
        match bounds with 
        | Some (lower, upper) -> Some (num.Next(lower, upper))
        | _ -> None

    let chosenNumber = Option.defaultValue 1 number

    let (evenOdd, randomFactor) = getNumInfo num chosenNumber

    printf "A random number has been chosen, the clues are
    - it is %s
    - it has a factor%s\n\n" evenOdd randomFactor
    printf "Guess the number: "
        
    let guessParsed, guess = System.Int32.TryParse(System.Console.ReadLine())
    let score = 
        match guessParsed, guess = chosenNumber with
        | true, true ->  "right"
        | true, false -> "wrong"
        | _ -> "invalid"

    printf "Your answer is %s, the correct answer was %d" score chosenNumber
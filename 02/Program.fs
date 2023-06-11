open System
open System.IO

let readFile fileName: seq<string> = File.ReadLines (Path.Combine(__SOURCE_DIRECTORY__, fileName))

type Choice =
    | Rock
    | Paper
    | Scissors
    
type Result =
    | Win
    | Draw
    | Loss
    
let getChoiceValue value =
    match value with
    | "A" | "X" -> Rock
    | "B" | "Y" -> Paper
    | "C" | "Z" -> Scissors
    | _   -> raise (Exception "Incorrect opponent input")
    
let getExpectedResultValue value =
    match value with
    | "X" -> Loss
    | "Y" -> Draw
    | "Z" -> Win
    | _   -> raise (Exception "Incorrect result input")
    
let getPlayersChoiceScore choice =
    match choice with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3
    
let getMoveResult opponentsChoice playersChoice =
    match (playersChoice, opponentsChoice) with
    | Rock, Scissors | Paper, Rock | Scissors, Paper -> Win
    | o, p -> if o = p then Draw else Loss
    
let getMoveResult2 (opponentsChoice, expectedResult) =
    match (opponentsChoice, expectedResult) with
    | Rock, Loss     -> Rock, Scissors
    | Rock, Win      -> Rock, Paper
    | Paper, Win     -> Paper, Scissors
    | Paper, Loss    -> Paper, Rock
    | Scissors, Win  -> Scissors, Rock
    | Scissors, Loss -> Scissors, Paper
    | x, _           -> x, x
    
let getMoveScore result =
    match result with
    | Win  -> 6
    | Draw -> 3
    | Loss -> 0
    
let getChoiceScore (opponentsChoice, playersChoice) =
    let playerChoiceScore = getPlayersChoiceScore playersChoice
    let moveScore = getMoveScore (getMoveResult opponentsChoice playersChoice)
    
    moveScore + playerChoiceScore

let parseInput fileName parseSecondInput = 
    let file = readFile fileName
    file
    |> Seq.map (fun item -> item.Split " ")
    |> Seq.map (fun item -> (getChoiceValue item[0], parseSecondInput item[1]))  

let question1 fileName =
    let parsedInput = parseInput fileName getChoiceValue
    
    parsedInput
    |> Seq.map getChoiceScore
    |> Seq.sum
    
    
let question2 fileName =
     let parsedInput = parseInput fileName getExpectedResultValue
     
     parsedInput
     |> Seq.map getMoveResult2
     |> Seq.map getChoiceScore
     |> Seq.sum
   
    
let result1 = question1 "input.txt"
printfn $"{result1}"

let result2 = question2 "input.txt"
printfn $"{result2}"
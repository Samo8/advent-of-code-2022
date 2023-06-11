open System
open System.IO

let readFile fileName = File.ReadAllText (Path.Combine(__SOURCE_DIRECTORY__, fileName))
    
let parseFile fileName =
    let file = readFile fileName
    
    file
    |> fun content -> content.Split([|"\n\n"|], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map (fun section ->
        section.Split('\n')
        |> Array.map (fun x ->
            match Int32.TryParse x with
            | true, n -> n
            | _       -> 0)
        |> Array.sum)

let question1 fileName =
    let calories = parseFile fileName
    
    calories
    |> Seq.max

let question2 fileName =
    let calories = parseFile fileName
    
    calories
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.sum
    
let result1 = question1 "input.txt"
printfn $"Question n.1 answer: {result1}"

let result2 = question2 "input.txt"
printfn $"Question n.2 answer: {result2}"
module ICPC
open System

let commaSprinkler input =
    let firstCheck = Char.IsLetter(input.[0]), input.[input.length -1]=".", input.length>1
    let rec checker input count = 
        match input.[count]=" " || input.[count]="," || input.[count]="." || Char.IsLower(input.[count]) with
        | false -> None
        | _ -> 
            match count = input.length -1 with
            | true -> input
            | _ -> checker input (count+1)
    match firstCheck with
    | true,true,true -> 
    | _ -> None

    let rec preReplacer input preIndex count word= 
        let myArr = input.IndexOf(",") //checks for index of comma
        let newInput = input.[preIndex..myArr-1]
        let myStr = newInput.Split(" ")
        let preWord = myStr.[myStr.Length -1]
        match count = myArr-1 with
        | true -> preReplacer input myArr 0 word
        | _ -> 
            match myStr.[count] = preWord with
            | false -> preReplacer input ((myStr.[count]).length +1) (count+1) (word+myStr.[count] + " ")
            | _ -> preReplacer input ((myStr.[count]).length +1) (count+1) (word+"," + myStr.[count] + " ")
 


        

let rivers input =
    let len = input.Split(" ")
    match len.length>1, Char.IsLetter(input,0),input.[input.length-1]="." with
    | (true,true, true) -> checker input 1
    | _ -> None 
    let rec checker input count =
        match Char.IsLetter(input,count) || (input.[count]=" " && input.[count+1]!=" ") with
        | false -> None
        | _ -> 
            match count = input.length -1 with
            | true -> input
            | _ -> checker input (count+1)
    failwith "Not implemented"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code

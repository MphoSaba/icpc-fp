module ICPC
open System

let commaSprinkler (input:string) = 
    let rec mined (input:string) count = 
        match count=input.Length-1 with
        | true -> Some input
        | false ->
            match Char.IsLower(input,count) || input.[count]=',' || input.[count]=' ' || input.[count]='.' with
            |true -> 
                match (Char.IsLower(input,count) || (input.[count]=' ' && Char.IsLower(input,count+1)) || (input.[count]=',' && input.[count+1]=' ') || (input.[count]='.' && input.[count+1]=' ')) with
                | true -> mined input (count+1)
                | false -> None
            | false -> None 
    match (input.Length>1) with
    | true -> 
        match (input.[input.Length-1]='.') && Char.IsLower(input,0) with
        | true ->(mined input 0)
        | false -> None
    | false -> None

let rivers input =
    let rec mined (xs: string list)=
        match xs with
        | [] -> true
        | x::rest ->
            match (x.Length)<81 with
            | true -> mined rest
            | false -> false
    let rec checker (input: string) count =
        match count=input.Length with
        | true -> 
            let changed = input.Split(' ') |> List.ofSeq
            match changed.Length>1 with
            | true ->   
                match mined changed with
                | true->
                    match input.[0]<>' ' || input.[input.Length-1]<>' ' with
                    | true -> None
                    | false -> Some input
                | _ -> None
            | false -> None
        | false ->
            match (Char.IsLetter(input,count) || (input.[count]=' ')) with
            | true -> checker input (count+1)
            | false -> None
    checker input 0  
        
[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code

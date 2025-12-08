open System.IO

let data = File.ReadAllText "../../day08.txt"
// let data = File.ReadAllText "../../day08-example.txt"

data |> Day08.part2 |> printfn "%A"

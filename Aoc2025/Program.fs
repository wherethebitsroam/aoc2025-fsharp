open System.IO

let data = File.ReadAllText "../../day09.txt"
// let data = File.ReadAllText "../../day09-example.txt"

data |> Day09.part2 |> printfn "%A"

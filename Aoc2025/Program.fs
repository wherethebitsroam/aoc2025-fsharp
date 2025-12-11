open System.IO

let data = File.ReadAllText "../../day11.txt"
// let data = File.ReadAllText "../../day11-example.txt"

data |> Day11.part2 |> printfn "%A"

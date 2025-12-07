open System.IO

let data = File.ReadAllText "../../day07.txt"
// let data = File.ReadAllText "../../day07-example.txt"

data |> Day07.part2 |> printfn "%A"

open System.IO

let data = File.ReadAllText "../../day06.txt"

// let data =
//     """123 328  51 64
//  45 64  387 23
//   6 98  215 314
//     *   +   *   +
//     """

data |> Day06.part2 |> printfn "%A"

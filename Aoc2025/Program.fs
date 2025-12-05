open System.IO

let data = File.ReadAllText "../../day05.txt"

// let data =
//     """3-5
//     10-14
//     16-20
//     12-18

//     1
//     5
//     8
//     11
//     17
//     32
// """

data |> Day05.part2 |> printfn "%A"

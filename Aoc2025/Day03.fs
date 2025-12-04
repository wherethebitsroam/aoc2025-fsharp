module Day03

open System

let parseLine (s: string) =
    s |> Seq.map (fun x -> int (x - '0')) |> Seq.toList

let parse (s: string) =
    s.Trim().Split '\n' |> Seq.map parseLine


let rec solve' (acc: int64) (pos: int) (l: List<int>) =
    if pos < 0 then
        acc
    else
        let digit = List.take (List.length l - pos) l |> List.max
        let rest = l |> List.skipWhile (fun x -> x <> digit) |> List.tail
        solve' (acc + int64 digit * pown 10L pos) pos rest

let solve (digits: int) (l: List<int>) = solve' 0L (digits - 1) l

let part1 (s: string) =
    s |> parse |> Seq.map (solve 2) |> Seq.sum

let part2 (s: string) =
    s |> parse |> Seq.map (solve 12) |> Seq.sum

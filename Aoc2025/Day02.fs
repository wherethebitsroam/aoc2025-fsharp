module Day02

open FParsec

module Parser =
    let range =
        parse {
            let! minVal = pint64
            do! skipChar '-'
            let! maxVal = pint64
            return minVal, maxVal
        }

    let parser = sepBy range (pstring ",")

    let parse (s: string) =
        match run parser s with
        | Success(result, _, _) -> result
        | Failure(error, _, _) -> failwith error

let len (x: int64) = String.length (x.ToString())

let topHalf (x: int64) =
    let l = len x
    let shift = l - l / 2
    x / pown 10L shift

let double (x: int64) =
    let l = len x
    x + x * pown 10L l

let rec invalid sum min max test =
    let d = double test

    if d > max then sum
    elif d >= min then invalid (sum + d) min max (test + 1L)
    else invalid sum min max (test + 1L)

let invalids (min: int64, max: int64) = invalid 0L min max (topHalf min)

let part1 (s: string) =
    s |> Parser.parse |> Seq.map invalids |> Seq.sum

module Part2 =
    let rec allEqual x =
        match x with
        | [] -> true
        | a :: [] -> true
        | a :: b :: rest -> if a = b then allEqual (b :: rest) else false

    let can_chunk (s: string) (size: int) =
        s |> Seq.chunkBySize size |> Seq.toList |> allEqual

    let is_invalid (x: int64) =
        let s = x.ToString()
        let maxChunk = String.length s / 2

        seq { 1..maxChunk } |> Seq.exists (can_chunk s)

    let rec check (max: int64) (acc: int64 list) (test: int64) =
        if test > max then
            acc
        elif is_invalid test then
            check max (test :: acc) (test + 1L)
        else
            check max acc (test + 1L)

    let invalids (min: int64, max: int64) = check max [] min

let part2 (s: string) =
    s |> Parser.parse |> Seq.collect Part2.invalids |> Seq.sum

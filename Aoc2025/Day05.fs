module Day05

open System

let parseRange (s: string) =
    let parts = s.Split '-'
    int64 (parts[0]), int64 (parts[1])

let parseRanges (s: string) =
    s.Split '\n' |> Array.map parseRange |> List.ofArray

let parse (s: string) =
    let parts = s.Trim().Split "\n\n"
    parseRanges parts[0], parts[1].Split '\n' |> Array.map Int64.Parse |> List.ofArray

let isFresh ranges ingredient =
    ranges |> List.exists (fun (l, h) -> l <= ingredient && ingredient <= h)

// If I were smart, I would merge the ranges and sort
// both lists, then go through once
let part1 (s: string) =
    let ranges, ingredients = s |> parse
    ingredients |> List.filter (isFresh ranges) |> List.length

type Event =
    | Start
    | Stop

let merger (start, depth, acc) (v, ev) =
    let depth =
        match ev with
        | Start -> depth + 1
        | Stop -> depth - 1

    if depth = 0 then
        // we hit depth 0, so it should be the end of a range
        // throw an error if we don't have a start point
        match start with
        | Some s -> None, 0, (s, v) :: acc
        | None -> failwith "wtf"
    else
        // we are either starting or continuing a range
        // set start to `v` if it does not already have a value
        start |> Option.orElse (Some v), depth, acc

let merge ranges =
    ranges
    |> List.collect (fun (l, h) -> [ l, Start; h, Stop ])
    |> List.sort
    |> List.fold merger (None, 0, [])
    |> fun (_, _, x) -> x
    |> List.rev

let size (l, h) = h - l + 1L

let part2 (s: string) =
    s |> parse |> fst |> merge |> List.map size |> List.sum

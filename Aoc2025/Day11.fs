module Day11

open FParsec

module Parser =
    // aaa: you hhh
    let device = many1Satisfy isAsciiLower

    let row =
        pipe2 (device .>> skipChar ':' .>> spaces) (sepBy1 device (skipChar ' ') .>> newline) (fun a b -> a, b)

    let parser = many1 row .>> spaces

    let parse (s: string) =
        match run parser s with
        | Success(result, _, _) -> result
        | Failure(error, _, _) -> failwith error

let rec follow (m: Map<string, list<string>>) path current =
    let nexts = m |> Map.find current
    nexts

let part1 (s: string) =
    let m = s |> Parser.parse |> Map.ofList
    follow m [] "you"

let part2 (s: string) = s |> Parser.parse

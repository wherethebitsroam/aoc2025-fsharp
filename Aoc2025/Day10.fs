module Day10

open FParsec

module Parser =
    // [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
    let indicator = skipChar '[' >>. many1 (anyOf ".#") .>> skipChar ']'
    let numList = sepBy1 pint32 (skipChar ',')
    let button = skipChar '(' >>. numList .>> skipChar ')'
    let joltage = skipChar '{' >>. numList .>> skipChar '}'

    let row =
        pipe3 (indicator .>> spaces) (many1 (button .>> spaces)) (joltage .>> newline) (fun i bs j -> i, bs, j)

    let parser = many1 row .>> spaces

    let parse (s: string) =
        match run parser s with
        | Success(result, _, _) -> result
        | Failure(error, _, _) -> failwith error

let indicatorToInt (is: list<char>) =
    is
    |> List.indexed
    |> List.fold (fun acc (index, v) -> if v = '#' then acc + (1 <<< index) else acc) 0

let buttonToInt (bs: list<int32>) =
    bs |> List.fold (fun acc b -> acc + (1 <<< b)) 0

let solve (indictor: int) (buttons: list<int>) =
    let rec solve' (seen: Set<int>) (lights: list<int>) (count: int) =
        // generate the next value of lights
        let lights =
            lights
            // apply each of the buttons to the lights
            |> List.collect (fun c -> buttons |> List.map (fun b -> b ^^^ c))
            // only keep those we've not seen
            |> List.filter (fun c -> Set.contains c seen |> not)
        // see if we're done
        if lights |> List.contains indictor then
            count
        else
            solve' (seen + Set.ofList lights) lights (count + 1)


    solve' ([ 0 ] |> Set.ofList) [ 0 ] 1

let part1 (s: string) =
    s
    |> Parser.parse
    |> List.map (fun (i, bs, _) -> solve (indicatorToInt i) (bs |> List.map buttonToInt))
    |> List.sum

let part2 (s: string) = s |> Parser.parse

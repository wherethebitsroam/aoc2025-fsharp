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

let follow (m: Map<string, list<string>>) (src: string) (dst: string) =
    let rec follow' acc paths =
        printfn "%A %A" (List.length acc) (List.length paths)

        match paths with
        | [] -> acc
        | paths ->
            let nexts =
                paths
                |> List.collect (fun path ->
                    let curr = List.head path

                    if curr = "out" then
                        []
                    else
                        let nextDevices =
                            match m |> Map.tryFind curr with
                            | Some x -> x
                            | None -> failwithf $"Not found `{curr}`"

                        nextDevices |> List.map (fun next -> next :: path))

            let complete, rest = nexts |> List.partition (fun path -> List.head path = dst)

            follow' (List.append acc complete) rest

    follow' [] [ [ src ] ]

let part1 (s: string) =
    let m = s |> Parser.parse |> Map.ofList
    follow m "you" "out" |> List.length

let part2 (s: string) =
    let m = s |> Parser.parse |> Map.ofList

    follow m "fft" "dac"
// |> List.filter (fun path -> path |> List.contains "dac")
// |> List.filter (fun path -> path |> List.contains "fft")
// |> List.length

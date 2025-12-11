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
    let rec follow' seen curr =

        match m |> Map.tryFind curr with
        | None -> seen |> Map.add curr 0, 0
        | Some nexts ->
            let seen, count =
                nexts
                |> List.fold
                    (fun (seen, count) next ->
                        let seen, x =
                            match seen |> Map.tryFind next with
                            | Some x -> seen, x
                            | None -> follow' seen next

                        seen, count + x)
                    (seen, 0)

            // insert into the map
            seen |> Map.add curr count, count

    follow' ([ (dst, 1) ] |> Map.ofList) src

let part1 (s: string) =
    let m = s |> Parser.parse |> Map.ofList
    follow m "you" "out"

let part2 (s: string) =
    let m = s |> Parser.parse |> Map.ofList

    let svr_dac = follow m "svr" "dac" |> snd |> int64
    printfn "svr_dac: %d" svr_dac
    let svr_fft = follow m "svr" "fft" |> snd |> int64
    printfn "svr_fft: %d" svr_fft
    let fft_dac = follow m "fft" "dac" |> snd |> int64
    printfn "fft_dac: %d" fft_dac
    let fft_out = follow m "fft" "out" |> snd |> int64
    printfn "fft_out: %d" fft_out
    let dac_fft = follow m "dac" "fft" |> snd |> int64
    printfn "dac_fft: %d" dac_fft
    let dac_out = follow m "dac" "out" |> snd |> int64
    printfn "dac_out: %d" dac_out

    svr_dac * dac_fft * fft_out + svr_fft * fft_dac * dac_out

module Day06

open System

let parse (s: string) =
    let lines =
        s.Trim().Split "\n"
        |> Seq.toList
        |> List.map (fun l -> l.Trim().Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Seq.toList)

    match lines |> List.rev with
    | [] -> failwith "wtf"
    | symbols :: numbers ->
        let nums =
            numbers |> List.map (fun l -> l |> List.map Int64.Parse) |> List.transpose

        List.zip symbols nums

let mul a b = a * b

let apply_op op (nums: list<int64>) =
    match op with
    | "+" -> nums |> List.sum
    | "*" -> nums |> List.fold mul 1
    | x -> failwithf "bad op: %A" x

let part1 (s: string) =
    s |> parse |> List.map (fun (op, nums) -> apply_op op nums) |> List.sum

let extend len ls =
    let l = ls |> List.length
    let to_add = List.init (len - l) (fun _ -> ' ')
    List.append ls to_add

let make_even ls =
    let max = ls |> List.map List.length |> List.max
    ls |> List.map (extend max)

let split_empty ls =
    let folder (acc, cur) s =
        if s = "" then cur :: acc, [] else acc, Int64.Parse s :: cur

    // remember to add the in progress group :)
    ls |> List.fold folder ([], []) |> (fun (acc, cur) -> cur :: acc |> List.rev)

let parse2 (s: string) =
    let lines = s.Trim().Split "\n" |> Seq.toList

    match lines |> List.rev with
    | [] -> failwith "wtf"
    | symbols :: numbers ->
        let syms =
            symbols.Trim().Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Seq.toList

        let nums =
            numbers
            // now that we've popped the symbols, rev to the right order again
            |> List.rev
            // split into chars
            |> List.map (fun s -> s |> Seq.toList)
            // add spaces to the end to get even rows
            |> make_even
            // transpose columns to rows
            |> List.transpose
            // join the chars to make values
            |> List.map (fun cs -> System.String.Join("", cs).Trim())
            // group the numbers splitting on the empty string
            |> split_empty

        List.zip syms nums

let part2 (s: string) =
    s |> parse2 |> List.map (fun (op, nums) -> apply_op op nums) |> List.sum

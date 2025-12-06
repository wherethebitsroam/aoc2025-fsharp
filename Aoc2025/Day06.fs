module Day06

open System

let split_ws (s: string) =
    s.Trim().Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Seq.toList

let process_num_rows (rows: list<string>) =
    rows
    |> List.map (fun row -> row |> split_ws |> List.map Int64.Parse)
    |> List.transpose

let parse (process_num_rows: list<string> -> list<list<int64>>) (s: string) =
    let lines = s.Trim().Split "\n" |> Seq.toList

    match lines |> List.rev with
    | [] -> failwith "wtf"
    | symbols :: numbers ->
        let syms = symbols |> split_ws
        let nums = numbers |> List.rev |> process_num_rows
        List.zip syms nums

let mul a b = a * b

let apply_op op (nums: list<int64>) =
    match op with
    | "+" -> nums |> List.sum
    | "*" -> nums |> List.fold mul 1
    | x -> failwithf "bad op: %A" x

let calc ls =
    ls |> List.map (fun (op, nums) -> apply_op op nums) |> List.sum

let part1 (s: string) = s |> parse process_num_rows |> calc

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

let process_nums2 ns =
    ns
    // split each line into chars
    |> List.map (fun s -> s |> Seq.toList)
    // add spaces to the end if required to get even rows
    |> make_even
    // transpose columns to rows
    |> List.transpose
    // join the chars to make values
    |> List.map (fun cs -> System.String.Join("", cs).Trim())
    // group the numbers splitting on the empty string
    |> split_empty

let part2 (s: string) = s |> parse process_nums2 |> calc

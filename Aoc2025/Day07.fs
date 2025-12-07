module Day07

open System

let parseRow (s: string) =
    s
    |> Seq.indexed
    |> Seq.choose (fun (i, c) -> if c = '^' then Some i else None)
    |> Set.ofSeq

let parse (s: string) =
    match s.Trim().Split '\n' |> Array.toList with
    | [] -> failwith "wtf"
    | first :: rest ->
        let start = first |> Seq.findIndex (fun x -> x = 'S')
        let splitters = rest |> List.map parseRow
        start, splitters

type State = { beams: Set<int>; splits: int }

let part1 (s: string) =
    let start, rows = s |> parse
    let beams = [ start ] |> Set.ofList

    let splitAt beams i =
        beams |> Set.remove i |> Set.add (i + 1) |> Set.add (i - 1)

    let folder (state: State) (splitters: Set<int>) =
        // this is the number of splits for this row
        let splits = Set.intersect state.beams splitters |> Set.count

        // update to the new state
        let beams = splitters |> Set.fold splitAt state.beams

        { beams = beams
          splits = state.splits + splits }

    rows |> List.fold folder { beams = beams; splits = 0 } |> (fun x -> x.splits)

let part2 (s: string) =
    let start, rows = s |> parse
    let beams = [ start, 1L ] |> Map.ofList

    // given an option, increment by or initialize to x
    let inc x opt =
        Some(opt |> Option.map (fun o -> o + x) |> Option.defaultValue x)

    let splitAt map i p =
        map |> Map.remove i |> Map.change (i - 1) (inc p) |> Map.change (i + 1) (inc p)

    // state is a map of beam positions to the number of ways of getting there
    let folder (state: Map<int, int64>) (splitters: Set<int>) =
        let folder map splitter =
            match state |> Map.tryFind splitter with
            | None -> map
            | Some p -> splitAt map splitter p

        splitters |> Set.fold folder state

    rows |> List.fold folder beams |> Map.values |> Seq.sum

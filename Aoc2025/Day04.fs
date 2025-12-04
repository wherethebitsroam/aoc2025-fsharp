module Day04

open System

type Point = { x: int; y: int }

module Point =
    let add p d = { x = p.x + d.x; y = p.y + d.y }

    let adjacent p =
        let d = add p

        [ d { x = -1; y = -1 }
          d { x = -1; y = 0 }
          d { x = -1; y = 1 }
          d { x = 0; y = -1 }
          d { x = 0; y = 1 }
          d { x = 1; y = -1 }
          d { x = 1; y = 0 }
          d { x = 1; y = 1 } ]

let parseLine (row: int, s: string) =
    s
    |> Seq.indexed
    |> Seq.choose (fun (c, x) -> if x = '@' then Some { y = row; x = c } else None)

let parse (s: string) =
    s.Trim().Split '\n' |> Seq.indexed |> Seq.collect parseLine |> Set.ofSeq

let accessible set p =
    let rolls =
        Point.adjacent p |> List.filter (fun n -> Set.contains n set) |> List.length

    rolls < 4

let part1 (s: string) =
    let set = s |> parse
    set |> Set.filter (accessible set) |> Set.count

let rec solve set =
    let next = set |> Set.filter (fun p -> accessible set p |> not)
    if Set.count set = Set.count next then set else solve next

let part2 (s: string) =
    let set = s |> parse
    let final = solve set
    Set.count set - Set.count final

module Day01

open System

let parseLine (s: string) =
    let lr = s[0]
    let num = Int32.Parse s[1..]

    match lr with
    | 'L' -> -num
    | 'R' -> num
    | _ -> failwith "bad direction"

let move (cur, zeros) elem =
    let next = (cur + 100 + elem) % 100
    next, if next = 0 then zeros + 1 else zeros

let part1 (s: string) =
    s.Trim().Split '\n' |> Seq.map parseLine |> Seq.fold move (50, 0)

let move2 (cur, zeros) elem =
    let loops = abs (elem / 100)
    let rem = elem % 100
    let next = cur + rem

    if next >= 100 then
        next - 100, zeros + loops + 1
    elif next < 0 then
        next + 100, zeros + loops + (if cur > 0 then 1 else 0)
    elif next = 0 then
        next, zeros + loops + 1
    else
        next, zeros + loops

let part2 (s: string) =
    s.Trim().Split '\n' |> Seq.map parseLine |> Seq.fold move2 (50, 0)

module Day09

open System
open System.Diagnostics
open SharpVG

let parseRow (s: string) =
    match s.Split ',' with
    | [| x; y |] -> int x, int y
    | _ -> failwith "sad"

let parse (s: string) =
    s.Trim().Split '\n' |> Array.toList |> List.map parseRow

// create all unique pairs from the list of points
let combinations ls =
    let a = ls |> Array.ofList

    seq {
        for i in 0 .. (Array.length a - 2) do
            for j in (i + 1) .. (Array.length a - 1) do
                yield a[i], a[j]
    }
    |> List.ofSeq

let size (ax: int, ay: int) (bx: int, by: int) =
    int64 (abs (ax - bx) + 1) * int64 (abs (ay - by) + 1)

let part1 (s: string) =
    s |> parse |> combinations |> List.map (fun (a, b) -> size a b) |> List.max

type VerticalEdge = { x: int; y_lo: int; y_hi: int }
type HorizontalEdge = { y: int; x_lo: int; x_hi: int }

let edges (points: list<int * int>) =
    let start = points |> List.head

    let edges = List.append points [ start ] |> List.pairwise

    let vertical =
        edges
        |> List.choose (fun ((x1, y1), (x2, y2)) ->
            if x1 = x2 then
                Some
                    { x = x1
                      y_lo = min y1 y2
                      y_hi = max y1 y2 }
            else
                None)

    let horizontal =
        edges
        |> List.choose (fun ((x1, y1), (x2, y2)) ->
            if x1 = x2 then
                None
            else
                Some
                    { y = y1
                      x_lo = min x1 x2
                      x_hi = max x1 x2 })

    vertical, horizontal

// Possibly combine vertical edges:
// .#...
// .#.o.  No combining, we hit the middle of the edge
// .#...
//
// .#.#...      .......   2 tops or 2 bottoms
// .###.o.  or  .###.o.   These get removed
// .......      .#.#...
//
// ...#...      .#.....   1 top and 1 bottom
// .###.o.  or  .###.o.   These get combined into a single edge
// .#.....      ...#...
let rec combine (acc: list<VerticalEdge>) (vs: list<VerticalEdge>) =
    match vs with
    | a :: b :: rest ->
        if a.y_lo = b.y_lo || a.y_hi = b.y_hi then
            combine acc rest
        elif a.y_lo = b.y_hi then
            combine
                ({ x = a.x
                   y_lo = b.y_lo
                   y_hi = a.y_hi }
                 :: acc)
                rest
        elif b.y_lo = a.y_hi then
            combine
                ({ x = b.x
                   y_lo = a.y_lo
                   y_hi = b.y_hi }
                 :: acc)
                rest
        else
            combine (a :: acc) (b :: rest)
    | [ a ] -> a :: acc
    | [] -> acc


let inside (vs: list<VerticalEdge>) (hs: list<HorizontalEdge>) (x: int, y: int) =
    let onV = vs |> List.exists (fun v -> v.x = x && v.y_lo <= y && y <= v.y_hi)
    let onH = hs |> List.exists (fun h -> h.y = y && h.x_lo <= x && x <= h.x_hi)

    if onV || onH then
        true
    else
        let vertical =
            vs
            |> List.filter (fun v -> v.x < x && v.y_lo <= y && y <= v.y_hi)
            |> combine []
            |> List.length

        vertical % 2 = 1


let allCornersInside isInside ((x1, y1), (x2, y2)) =
    // printfn "(%d,%d) -> (%d,%d) -> %d" x1 y1 x2 y2 (size (x1, y1) (x2, y2))
    // printfn "(%d,%d) -> %A" x1 y1 (isInside (x1, y1))
    // printfn "(%d,%d) -> %A" x1 y2 (isInside (x1, y2))
    // printfn "(%d,%d) -> %A" x2 y1 (isInside (x2, y1))
    // printfn "(%d,%d) -> %A" x2 y2 (isInside (x2, y2))
    isInside (x1, y1) && isInside (x1, y2) && isInside (x2, y1) && isInside (x2, y2)

let crossingEdges (vs: list<VerticalEdge>) (hs: list<HorizontalEdge>) ((x1, y1), (x2, y2)) =
    let min_x = min x1 x2
    let max_x = max x1 x2
    let min_y = min y1 y2
    let max_y = max y1 y2

    let vertical =
        vs
        |> List.exists (fun v -> min_x < v.x && v.x < max_x && v.y_hi > min_y && v.y_lo < max_y)

    let horizontal =
        hs
        |> List.exists (fun h -> min_y < h.y && h.y < max_y && h.x_hi > min_x && h.x_lo < max_x)

    vertical || horizontal

let svg (points: list<int * int>) =
    let style =
        Style.create (Color.ofName Colors.White) (Color.ofName Colors.Black) (Length.ofInt 1) 1.0 1.0

    let poly =
        points
        |> List.map Point.ofInts
        |> Polygon.ofList
        |> Element.createWithStyle style

    // (14914, 82125), (86096, 17667) -> bad
    let rect =
        Rect.create
            (Point.ofInts (14914, 17667))
            { Width = Length.ofInt (86096 - 14914)
              Height = Length.ofInt (82125 - 17667) }
        |> Element.createWithStyle (
            Style.create (Color.ofName Colors.Yellow) (Color.ofName Colors.Yellow) (Length.ofInt 1) 0.5 0.5
        )

    // (5786, 68749), (94817, 50191) -> good
    let rect =
        Rect.create
            (Point.ofInts (5786, 50191))
            { Width = Length.ofInt (94817 - 5786)
              Height = Length.ofInt (68749 - 50191) }
        |> Element.createWithStyle (
            Style.create (Color.ofName Colors.Orange) (Color.ofName Colors.Orange) (Length.ofInt 1) 0.5 0.5
        )

    let max_x = points |> List.map fst |> List.max
    let max_y = points |> List.map snd |> List.max

    [ poly; rect ]
    |> Svg.ofList
    |> Svg.withViewBox
        { Minimum = Point.origin
          Size =
            { Width = Length.ofInt max_x
              Height = Length.ofInt max_y } }

let part2 (s: string) =
    let points = s |> parse
    let vs, hs = points |> edges

    let isInside = inside vs hs

    // 3733912764 -> too high
    // ((14914L, 82125L), (86096L, 17667L), 4588384997L)

    points
    |> combinations
    |> List.filter (allCornersInside isInside)
    |> List.filter (fun corners -> crossingEdges vs hs corners |> not)
    |> List.map (fun (a, b) -> a, b, size a b)
    |> List.maxBy (fun (_, _, s) -> s)

let print (s: string) =
    let points = s |> parse
    points |> svg |> printf "%O"

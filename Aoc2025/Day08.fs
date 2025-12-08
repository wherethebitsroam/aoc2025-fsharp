module Day08

open System

type Point = { x: int64; y: int64; z: int64 }

module Point =
    let sld p1 p2 =
        pown (p1.x - p2.x) 2 + pown (p1.y - p2.y) 2 + pown (p1.z - p2.z) 2
        |> float
        |> sqrt

    let print p = $"{p.x},{p.y},{p.z}"

let parseRow (s: string) =
    match s.Split ',' with
    | [| x; y; z |] ->
        { x = int64 x
          y = int64 y
          z = int64 z }
    | _ -> failwith "sad"

let parse (s: string) =
    s.Trim().Split '\n' |> Array.toList |> List.map parseRow

// create all unique pairs from the list of points
let combinations ls =
    let rec com acc ls =
        match ls with
        | x :: rest ->
            let blah = rest |> List.map (fun l -> x, l)
            com (List.append acc blah) rest
        | [] -> acc

    com [] ls

let find (x: Point) (sets: Set<Set<Point>>) =
    sets |> Set.toList |> List.find (fun s -> s |> Set.contains x)

let addConnection (a: Point, b: Point) (sets: Set<Set<Point>>) =
    let a_set = sets |> find a
    let b_set = sets |> find b
    sets |> Set.remove a_set |> Set.remove b_set |> Set.add (Set.union a_set b_set)

let sortedByDist junctions =
    junctions
    |> combinations
    |> List.map (fun (a, b) -> (a, b), Point.sld a b)
    |> List.sortBy (fun (_, sld) -> sld)
    |> List.map fst

let junctionsSet junctions =
    // make a set of sets, with each Point in it's own set
    junctions |> List.map (fun p -> [ p ] |> Set.ofList) |> Set.ofList

let part1 (s: string) =
    let junctions = s |> parse
    let sets = junctions |> junctionsSet

    junctions
    |> sortedByDist
    |> List.take 1000
    // combine sets based on the connections
    |> List.fold (fun sets conn -> sets |> addConnection conn) sets
    |> Set.toList
    |> List.map Set.count
    |> List.sortDescending
    |> List.take 3
    |> List.fold (fun acc x -> acc * x) 1

// recursively group sets by connection until there is only
// a single set. Return the points from the final connection
let rec group (sets: Set<Set<Point>>) (ls: list<Point * Point>) =
    match ls with
    | [] -> failwith "reached the end"
    | conn :: rest ->
        let sets = sets |> addConnection conn
        if Set.count sets = 1 then conn else group sets rest

let part2 (s: string) =
    let junctions = s |> parse
    let sets = junctions |> junctionsSet
    junctions |> sortedByDist |> group sets |> (fun (p1, p2) -> p1.x * p2.x)

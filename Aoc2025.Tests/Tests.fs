module Tests

open System
open Xunit

let data: obj[] list =
    [ [| [ (1, 2) ]; [ (1, 2) ] |]
      [| [ (1, 2); (1, 2) ]; [ (1, 2) ] |]
      [| [ (1, 1) ]; [ (1, 1) ] |]
      [| [ (1, 1); (2, 2) ]; [ (1, 1); (2, 2) ] |]
      [| [ (1, 2); (2, 3) ]; [ (1, 3) ] |]
      [| [ (1, 2); (4, 5) ]; [ (1, 2); (4, 5) ] |] ]

[<Theory>]
[<MemberData(nameof (data))>]
let ``My test`` ranges expected =
    let r = Day05.merge ranges
    Assert.Equal<list<int * int>>(expected, r)

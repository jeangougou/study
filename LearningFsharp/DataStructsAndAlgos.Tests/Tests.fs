module Tests

open DataStructsAndAlgos
open System
open Xunit

[<Fact>]
let ``Sorting with recursive quicksort`` () =
  let rand = new System.Random()
  let data = List.init 10 (fun _ -> rand.Next())
  let actualSort = Sorting.QuickSort data
  let expectedSort = List.sort data
  List.zip actualSort expectedSort |> List.map (fun (a,b) -> Assert.Equal(a, b))
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

[<Fact>]
let ``Sorting with recursive idiomatic quicksort`` () =
  let rand = new System.Random()
  let data = List.init 10 (fun _ -> rand.Next())
  let actualSort = Sorting.IdiomaticFunctionalQuickSort data |> Seq.toList
  let expectedSort = List.sort data
  List.zip actualSort expectedSort |> List.map (fun (a,b) -> Assert.Equal(a, b))


[<Fact>]
let ``Sorting with bubblesort`` () =
  let rand = new System.Random()
  let data = List.init 10 (fun _ -> rand.Next())
  let actualSort = Sorting.BubbleSort (data |> List.toArray) |> Seq.toList
  let expectedSort = List.sort data
  List.zip actualSort expectedSort |> List.map (fun (a,b) -> Assert.Equal(a, b))

[<Fact>]
let ``Sorting with recursive idiomatic bubblesort`` () =
  let rand = new System.Random()
  let data = List.init 10 (fun _ -> rand.Next())
  let actualSort = Sorting.IdiomaticFunctionalBubbleSort (data) |> Seq.toList
  let expectedSort = List.sort data
  List.zip actualSort expectedSort |> List.map (fun (a,b) -> Assert.Equal(a, b))
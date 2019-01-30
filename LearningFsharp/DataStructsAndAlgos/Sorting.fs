module Sorting

let rec QuickSort = function
  | [] -> []
  | n::ns -> let lessthan, gr8terEqual = List.partition ((>) n) ns
             QuickSort lessthan @ n :: QuickSort gr8terEqual

let rec IdiomaticFunctionalQuickSort (pxs:seq<_>) =
  seq {
    match Seq.toList pxs with
    | p::xs -> let lessthan, greaterThan = List.partition ((>=) p) xs
               yield! IdiomaticFunctionalQuickSort lessthan; yield p; yield! IdiomaticFunctionalQuickSort greaterThan
    | _ -> ()
  }
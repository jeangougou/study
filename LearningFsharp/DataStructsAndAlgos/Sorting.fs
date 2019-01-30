module Sorting

let rec QuickSort = function
  | [] -> []
  | n::ns -> let lessthan, gr8terEqual = List.partition ((>) n) ns
             QuickSort lessthan @ n :: QuickSort gr8terEqual


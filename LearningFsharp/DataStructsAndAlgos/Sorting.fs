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

let swap (left : 'a  byref) (right: 'a byref) =
  let temp = left
  left <- right
  right <- temp

let BubbleSort array =
  let rec loop (array : 'arr []) =
    let mutable swaps = 0
    for i = 0 to array.Length - 2 do
      if array.[i] > array.[i+1] then
        swap &array.[i] &array.[i+1]
        swaps <- swaps+1
    if swaps > 0 then loop array else array
  loop array
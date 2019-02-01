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

// still relies in mutability
let BubbleSort array =
  let rec loop (array : 'arr []) =
    let mutable swaps = 0
    for i = 0 to array.Length - 2 do
      if array.[i] > array.[i+1] then
        swap &array.[i] &array.[i+1]
        swaps <- swaps+1
    if swaps > 0 then loop array else array
  loop array

// helper to get the highest number
let rec getHighest list =
  match list with
  | head1::head2::tail when head1 > head2 -> getHighest(head1::tail)
  | _::head2::tail -> getHighest(head2::tail)
  | head1::[] -> head1
  | _ -> failwith "Unrecognized pattern"

let IdiomaticFunctionalBubbleSort list =
  let rec innerBubbleSort sorted = function
  | [] -> sorted
  | l ->
    let h = getHighest l
    let (x, y) = List.partition (fun i -> i = h) l
    innerBubbleSort (x @ sorted) y
  innerBubbleSort [] list
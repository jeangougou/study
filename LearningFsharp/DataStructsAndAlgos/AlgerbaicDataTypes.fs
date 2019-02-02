module AlgebraicDataTypes

(* Custom version of list, through a product type*)
type 'a mylist =
  Empty
  | Cons of 'a * 'a mylist

let example1 = Cons(1, Cons ( 2, Cons(3, Empty)))

let rec length = function
  | Empty -> 0
  | Cons (_, tl) -> 1 + length tl

let example2 = length example1

// Sample option type
type 'a option =
  | None
  | Some of 'a
module SequenceSample

(*implementing a sequence

we expect the following signatures:

  val empty : 'a sequence
  val is_empty : 'a sequence -> bool
  val extend : 'a -> 'a sequence -> 'a sequence
  val first : 'a sequence -> 'a option
  val rest : 'a sequence -> 'a sequence option
  val index : int -> 'a sequence -> 'a option

any Cons (a, b) can be rewritten in a::b
The pure OCaml syntax is to explicit the constructor again.

*)

type 'a SequenceSample =
  Empty
| Cons of 'a * 'a SequenceSample

let empty = Empty

let isEmpty = function
  | Empty -> true
  | _ -> false

let Extend = fun e l -> Cons(e,l)

let First = function
  | Empty -> None
  | Cons (e,_) -> Some e

let Rest = function
  | Empty -> None
  | Cons (_, l) -> Some l

let rec Index n = function
  | Empty -> None
  | Cons (e,_) when n = 0 -> Some e
  | Cons (_, tl) when n > 0 -> Index (n-1) tl
  | _ -> None
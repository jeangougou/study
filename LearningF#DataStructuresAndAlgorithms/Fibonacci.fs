module Fibonacci

open System.Collections.Generic

let rec RecursiveFibonacci n =
  if n <= 2 then 1
  else RecursiveFibonacci (n-1) + RecursiveFibonacci (n-2)

let TailRecursiveFibonacci n =
  let rec tailRecFibonacci (n, x, y) =
    if (n = 0I) then x
    else tailRecFibonacci ((n-1I), y, (x+y))
  tailRecFibonacci (n, 0I, 1I)

let rec MemoizedFibonacci n =
  let cache = Dictionary<_,_>()
  let rec fibX = function
    | n when n = 0I -> 0I
    | n when n = 1I -> 1I
    | n -> fibX (n-1I) + fibX (n-2I)
  if cache.ContainsKey(n) then cache.[n]
  else
    let result = fibX n
    cache.[n] <- result
    result


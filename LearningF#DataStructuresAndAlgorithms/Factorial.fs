module Factorial

let IterativeFactorial x =
  let mutable n = x
  let mutable returnVal = 1
  while n >= 1 do
    returnVal <- returnVal * n
    n <- (n-1)
  returnVal

let rec RecursiveFactorial n =
  if n <= 1 then 1
  else n * RecursiveFactorial (n-1)

let rec PatternMatchedFactorial n =
  match n with
  | 0 | 1 -> 1
  | _ -> n * PatternMatchedFactorial (n-1)

let TailRecursiveFactorial n =
  let rec tailRecFact n accum =
    if n <= 1 then accum
    else tailRecFact (n - 1) (accum * n)
  tailRecFact n 1

let ContinuationBasedFactorial n =
  let rec contTailRecFact n f =
    if n <= 1 then f()
    else contTailRecFact (n-1) (fun () -> n * f())
  contTailRecFact n (fun () -> 1)

let FoldFactorial n = [1..n] |> List.fold (*) 1

let ReduceFactorial n = [1..n] |> List.reduce (*)
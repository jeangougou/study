module Hanoi

// f = from, t= to, x = intermediate state, n = number of pegs/disks
let rec TowerOfHanoi f x t n =
  if n > 0 then
    TowerOfHanoi f t x (n-1)
    printf "Move disc from %c to %c" f t
    TowerOfHanoi x f t (n-1)

(*
Well-founded coalgebras, revisited
http://www.cs.cornell.edu/~kozen/Papers/Theory.pdf
*)
let rec IdiomaticTowerOfHanoi n o d t =
  if n = 0 then [] else
    (IdiomaticTowerOfHanoi (n-1) o t d) @ [(o,d)] @ (IdiomaticTowerOfHanoi (n-1) t d o)


// n = number pegs, s = start, f = finish
let rec RecursiveTowerOfHanoi n s f =
  match n with
  | 0 -> []
  | _ -> let t = (6 - s - f)
         (RecursiveTowerOfHanoi (n-1) s t) @ [s,f] @ (RecursiveTowerOfHanoi (n-1) t f)

// test with:
// (RecursiveTowerOfHanoi 2 1 2) |> List.iter (fun (x,y) -> printf "Move disc from %A to %A" x y)
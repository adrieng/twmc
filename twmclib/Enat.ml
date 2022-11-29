type t = int

let to_string x =
  if x = 0 then "omega" else string_of_int (x - 1)

let view x =
  assert (x >= 0);
  if x = 0 then `Omega else `Fin (x - 1)

let of_int x =
  if x < 0 then invalid_arg "of_int" else x + 1

let omega = 0

let zero = 1

let to_int x = if x = omega then invalid_arg "to_int" else x - 1

let ( <= ) x y = (y = omega) || (x <> omega && x <= y)

let ( < ) x y = x <= y && not (x = y)

let compare a b =
  if a = b then 0 else if a <= b then -1 else 1

let succ x = if x = omega then omega else x + 1

let ( + ) x y = if x = omega || y = omega then omega else x + y - 1

(* (- k) should be left adjoint to (+ k), so that:

     x - k <= y  iff  x <= y + k.

   Thus x - k is the smallest number such that x <= (x - k) + k.

   Thus omega - omega = 0. *)
let ( - ) x y =
  match x, y with
  | _, 0 -> 1
  | 0, _ -> 0
  | x, y -> 1 + x - y

let ( * ) x y = if x = omega || y = omega then 0 else 1 + (x - 1) * (y - 1)

let max x y = if x = omega || y = omega then omega else max x y

let min x y =
  match x, y with
  | 0, x | x, 0 -> x
  | _ -> min x y

let raw_of_int x = x

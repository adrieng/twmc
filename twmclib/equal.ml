type 'a t = 'a -> 'a -> bool

let assoc_list eqk eqv xs ys =
  let rec assoc_remove_opt kx ys =
    match ys with
    | [] -> None
    | (ky, vy) :: ys ->
       if eqk kx ky
       then Some (vy, ys)
       else
         begin match assoc_remove_opt kx ys with
         | None -> None
         | Some (vy', ys) -> Some (vy', (ky, vy) :: ys)
         end
  in
  let rec aux xs ys =
    match ys, xs with
    | [], [] -> true
    | [], _ :: _ -> false
    | (kx, vx) :: xs, _ ->
       begin match assoc_remove_opt kx ys with
       | None -> false
       | Some (vy, ys) -> eqv vx vy && aux xs ys
       end
  in
  aux xs ys

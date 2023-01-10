(** We call {i eventually affine} the class of time warps, that is,
    join-preserving maps of ω+, whose graphs either become flat or of slope 1 at
    a {b finite} number. We call this number the {i phase} of the time warp.

    We describe an eventually affine map by its {i prefix}, which is the array
    of its values up to the phase {b excluded}, and what comes next: either the
    map reaches a plateau of a certain [height] with a final step ([`Plateau
    height]), or reaches its final straight line of slope 1. The latter case is
    represented by [`Slope final], where [final] is the sum of all values in the
    prefix plus one, precomputed for efficiency reasons.  *)

type t =
  {
    prefix : int array;
    period : [`Plateau of Enat.t | `Slope of int];
  }

let phase f =
  Array.length f.prefix

let eval f n =
  match Enat.view n with
  | `Omega ->
     begin match f.period with
     | `Plateau h -> h
     | `Slope _ -> Enat.omega
     end
  | `Fin i ->
     if i < phase f then Enat.of_int f.prefix.(i)
     else
       begin match f.period with
       | `Plateau h -> h
       | `Slope j -> Enat.of_int @@ j + (i - phase f)
       end

let print f =
  let open PPrint in
  group
    ((Array.to_seq f.prefix
     |> List.of_seq
     |> separate_map (break 1) OCaml.int)
     ^//^ match f.period with
          | `Plateau h -> Enat.print h ^//^ !^ "+0"
          | `Slope _ -> !^ "+1")

let equal = Stdlib.( = )

let extend points =
  let rec decompose i1 j1 = function
    | [] ->
       invalid_arg "of_points"
    | (i2, j2) :: points ->
       begin match Enat.(view i2, view j2) with
       | `Omega, `Omega ->
          [], i1 + 1, `Sloped
       | `Omega, `Fin _ ->
          [], i1 + 1, `Plateau j2
       | `Fin i2, `Omega ->
          [(i2 - 1, j1)], i2, `Plateau Enat.omega
       | `Fin i2, `Fin j2 ->
          let points, phase, final = decompose i2 j2 points in
          (i2, j2) :: points, phase, final
       end
  in
  let points, phase, period =
    List.sort_uniq (fun (i1, _) (i2, _) -> Enat.compare i1 i2) points
    |> decompose 0 0
  in
  let prefix = Array.make phase 0 in

  let rec fill i1 j1 = function
    | [] -> ()
    | (i2, j2) :: points ->
       for i = i1 + 1 to i2 - 1 do
         prefix.(i) <- min j2 (j1 + i - i1)
       done;
       prefix.(i2) <- j2;
       fill i2 j2 points
  in

  fill 0 0 points;

  { prefix; period = match period with
                     | `Plateau h -> `Plateau h
                     | `Sloped -> `Slope (Array.fold_left (+) 0 prefix); }

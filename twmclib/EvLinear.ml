type t =
  {
    u : int array;
    u_sum : int;
    v : Enat.t;
  }

let pp { u; v; _ } =
  let open PPrint in
  let int i = !^ (string_of_int i) in
  group (separate_map (break 1) int (Array.to_list u)
         ^^ !^ (Printf.sprintf "(%s)" Enat.(to_string v)))

let make u v =
  {
    u = Array.copy u;
    u_sum = Array.fold_left (+) 0 u;
    v;
  }

let of_points points =
  let points = (Enat.zero, Enat.zero) :: points in
  let points = List.sort_uniq (fun (a, _) (b, _) -> Enat.compare a b) points in
  let rec loop : int list -> (Enat.t * Enat.t) list -> int list * Enat.t =
    fun acc points ->
    match points with
    | [] ->
       (* Impossible since last and 0 are always in [points]. *)
       assert false
    | [(_, dlast)] ->
       List.rev acc, dlast
    | (i1, j1) :: (((i2, j2) :: _) as points) ->
       let open Enat in
       assert (i1 < i2);
       assert (j1 <= j2);
       (* assert (i1 < omega); *)
       (* assert (j1 < omega); *)
       let rec interpolate acc (i : Enat.t) (j : Enat.t) =
         if i2 = Enat.omega then List.rev acc, Enat.of_int 1
         else if j = Enat.omega then List.rev acc, Enat.omega
         else if i2 <= i then loop acc points
         else interpolate (Enat.to_int j :: acc) (succ i) (min (succ j) j2)
       in
       interpolate acc i1 j1
  in
  let u, v = loop [] points in
  { u = Array.of_list u; u_sum = 0; v; }

let eval_period p i =
  match Enat.view p.v with
  | `Omega -> Enat.omega
  | `Fin k -> Enat.of_int (i * k)

let eval p i =
  match Enat.view i, Enat.view p.v with
  | `Omega, `Fin 0 ->
     Enat.zero
  | `Omega, _ ->
     Enat.omega
  | `Fin i, `Omega when i >= Array.length p.u ->
     Enat.omega
  | `Fin i, `Fin k when i >= Array.length p.u ->
     Enat.of_int @@ p.u_sum + k * (i - Array.length p.u + 1)
  | `Fin i, _ ->
    let r = ref 0 in
    for j = 0 to i - 1 do
      r := !r + p.u.(j)
    done;
    Enat.of_int !r

let equal p1 p2 =
  p1.v = p2.v && Array.for_all2 ( = ) p1.u p2.u

let ( <= ) p1 p2 =
  (* FIXME *)
  assert false
  (* p1.last <= p2.last && p1.len = p2.len && *)
  (*   let rec loop i s1 s2 = *)
  (*     i >= p1.len *)
  (*     || let a = s1 + p1.u.(i) in *)
  (*        let b = s2 + p2.u.(i) in *)
  (*        a <= b && loop (i + 1) s1 s2 *)
  (*   in *)
  (*   loop 0 0 0 *)

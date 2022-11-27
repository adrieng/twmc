type enat =
  | Fin of int
  | Omega

type t =
  {
    u : int array;
    u_sum : int;
    v : enat;
  }

let pp_period = function
  | Fin i -> string_of_int i
  | Omega -> "w"

let pp { u; v; _ } =
  let open PPrint in
  let int i = !^ (string_of_int i) in
  group (separate_map (break 1) int (Array.to_list u)
         ^^ !^ (Printf.sprintf "(%s)" (pp_period v)))

let make u v =
  {
    u = Array.copy u;
    u_sum = Array.fold_left (+) 0 u;
    v;
  }

let of_points ~last points =
  (* let u = Array.make  *)
  (* let rec loop acc points = *)
  (*   match points with *)
  (*   | [] -> *)
  (*      (\* Impossible since last and 0 are always in [points]. *\) *)
  (*      assert false *)
  (*   | [(_, dlast)] -> *)
  (*      List.rev acc, dlast *)
  (*   | (a1, b1) :: ((a2, b2) :: _) as points -> *)
  (*      assert (a1 < a2); *)
  (*      assert (b1 < b2); *)
  (*      let rec loop2 acc i j = *)
  (*        if i >= a2 then loop acc points *)
  (*        else loop2 (j :: acc) (i + 1) (min (j + 1) b2) *)
  (*      in *)
  (*      loop2 acc a1 b1 *)
  (* in *)
  (* let u, v = *)
  (*   ((0, 0) :: points) *)
  (*   |> List.sort (fun (a1, b1) (a2, b2) -> Stdlib.compare a1 a2) *)
  (*   |> loop [] *)
  (* in *)
  (* { u = Array.of_list u; v; } *)
  assert false

let eval_period p i =
  match p.v with
  | Omega -> Omega
  | Fin k -> Fin (i * k)

let eval p i =
  match i, p.v with
  | Omega, Fin 0 ->
     Fin 0
  | Omega, _ ->
     Omega
  | Fin i, Omega when i >= Array.length p.u ->
     Omega
  | Fin i, Fin k when i >= Array.length p.u ->
     Fin (p.u_sum + k * (i - Array.length p.u + 1))
  | Fin i, _ ->
    let r = ref 0 in
    for j = 0 to i - 1 do
      r := !r + p.u.(j)
    done;
    Fin !r

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

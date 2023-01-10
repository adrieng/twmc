type t =
  {
    valuation : (Warp.id * Regular.t) list;
    point : int;
  }

let add ce x p = { ce with valuation = (x, p) :: ce.valuation; }

let dummy = { valuation = []; point = 0; }

let pp ({ valuation; point; } as cex) =
  let open PPrint in
  if cex == dummy then !^ "(dummy)"
  else
    let binding (x, t) =
      PPrint.prefix 2 1 (Warp.Print.id x ^^ !^ " =") (Regular.print t)
    in
    separate_map
      hardline
      (fun (k, v) -> group @@ prefix 2 1 (!^ k) v)
      [
        "values:", OCaml.list binding valuation;
        "discrepancy:", OCaml.int point;
      ]

let equal cex1 cex2 =
  cex1.point = cex2.point
  && Equal.assoc_list (=) Regular.equal cex1.valuation cex2.valuation

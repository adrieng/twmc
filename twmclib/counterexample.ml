type t =
  {
    valuation : (Warp.id * Compact.t) list;
    point : int;
  }

let dummy = { valuation = []; point = 0; }

let pp ({ valuation; point; } as cex) =
  let open PPrint in
  if cex == dummy then !^ "(dummy)"
  else
    let binding (x, t) = PPrint.prefix 2 1 (Warp.Print.id x) (Compact.pp t) in
    prefix 2 1 (!^ "values:")
      (surround_separate_map 2 1
         (!^ "[]") (!^ "[") (!^ ",") (!^ "]") binding valuation)
    ^^ prefix 2 1 (!^ "discrepancy:") (!^ (string_of_int point))

let equal cex1 cex2 =
  cex1.point = cex2.point
  && Equal.assoc_list (=) Compact.equal cex1.valuation cex2.valuation

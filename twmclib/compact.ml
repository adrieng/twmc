type t =
  {
    u : int array;
    len : int;
    last : int;
  }

let print { u; _ } =
  let open PPrint in
  let int i = !^ (string_of_int i) in
  group (separate_map (break 1) int (Array.to_list u) ^^ !^ "(0)")

let make u =
  let rec trim_trailing_zeroes i =
    assert (0 <= i && i <= Array.length u);
    if i = 0 then 0
    else if u.(i - 1) <> 0 then trim_trailing_zeroes (i - 1)
    else i
  in
  let len = trim_trailing_zeroes (Array.length u) in
  {
    u = Array.sub u 0 len;
    len;
    last = Array.fold_left ( + ) 0 u;
  }

let eval p i =
  if i >= p.len then p.last else
    let r = ref 0 in
    for j = 0 to i do
      r := !r + p.u.(j)
    done;
    !r

let equal p1 p2 =
  p1.last = p2.last && Array.for_all2 ( = ) p1.u p2.u

type counterexample =
  {
    valuation : (Warp.id * t) list;
    point : int;
  }

let print_counterexample { valuation; point; } =
  let open PPrint in
  let binding (x, t) =
    PPrint.prefix 2 1 (Warp.Print.id x) (print t)
  in
  prefix 2 1 (!^ "values:")
    (surround_separate_map 2 1
       (!^ "[]") (!^ "[") (!^ ",") (!^ "]") binding valuation)
  ^^ prefix 2 1 (!^ "discrepancy:") (!^ (string_of_int point))

let equal_counterexample cex1 cex2 =
  cex1.point = cex2.point
  && Equal.assoc_list (=) equal cex1.valuation cex2.valuation

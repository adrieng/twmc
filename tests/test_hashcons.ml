open Twmclib

module V = Symbol.Make()

module MS = struct
  type 'a t =
    | V of (V.t [@compare V.compare] [@equal V.equal])
    | U
    | M of 'a * 'a
  [@@deriving ord, hash]

  let hash_fold = hash_fold_t

  let map f a =
    match a with
    | V x -> V x
    | U -> U
    | M (b1, b2) -> M (f b1, f b2)

  let pp pp_term a =
    let open PPrint in
    match a with
    | V x -> !^ (V.to_string x)
    | U -> !^ "U"
    | M (b1, b2) ->
       let d1 = pp_term b1 in
       let d2 = pp_term b2 in
       prefix 2 1 (!^ "M") (parens @@ group @@ d1 ^^ comma ^^  break 1 ^^ d2)
end

module T = Hashcons.Share(MS)
let term, different_term =
  let ff = Print.PPrint.to_fmt T.pp in
  Alcotest.testable ff T.equal,
  Alcotest.testable ff (fun x y -> not (T.equal x y))

let u () = T.make MS.U
let v x = T.make MS.(V x)
let m a b = T.make MS.(M (a, b))

let x = V.fresh ~name:"x" ()
let y = V.fresh ~name:"y" ()

let test_share_var () =
  Alcotest.check term "same terms" (v x) (v x)

let test_share_u () =
  Alcotest.check term "same terms" T.(make U) T.(make U)

let test_share_comp () =
  Alcotest.check term "same terms"
    (m (u ()) (m (v x) (v y)))
    (m (u ()) (m (v x) (v y)))

let test_distinct_var () =
  Alcotest.check different_term "distinct terms" (v x) (v y)

let test_distinct_comp () =
  Alcotest.check different_term "distinct terms"
    (m (u ()) (m (u ()) (v y)))
    (m (u ()) (m (v x) (v y)))

let () =
  let open Alcotest in
  run "Hashcons"
    [
      "same", [
        test_case "var" `Quick test_share_var;
        test_case "noparam" `Quick test_share_u;
        test_case "param" `Quick test_share_comp;
      ];
      "distinct", [
        test_case "var" `Quick test_distinct_var;
        test_case "param" `Quick test_distinct_comp;
      ];
    ]

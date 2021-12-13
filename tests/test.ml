open Twmclib

let x_id = Warp.V.fresh ~name:"x" ()
let x = Warp.Var x_id

let result, result_cex =
  let pp = Print.to_fmt Result.print in
  Alcotest.testable pp (Result.equal ~consider_counterexamples:false),
  Alcotest.testable pp (Result.equal ~consider_counterexamples:true)

let test_le () =
  let cases =
    let open Result in
    let open Warp in
    let open Infix in
    [
      x, x, Valid;
      x * x, x * x, Valid;
    ]
  in
  List.map
    (fun (s, t, res) ->
      let rel = Warp.Le (s, t) in
      let repr = Print.to_string (Warp.Print.rel rel) in
      Alcotest.test_case repr `Quick
        (fun () -> Alcotest.check result repr Result.(solve rel) res))
    cases;;

let () =
  let open Alcotest in
  run "TWMC"
    [
      "le", test_le ();
    ]

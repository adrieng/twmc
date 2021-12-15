open Twmclib

let result =
  let pp fmt r =
    match r with
    | `Valid ->
       Format.fprintf fmt "valid"
    | `Invalid cex ->
       Format.fprintf fmt "invalid: %a" (Print.to_fmt Counterexample.pp) cex
  and equal r1 r2 =
    match r1, r2 with
    | `Valid, `Valid | `Invalid _, `Invalid _ -> true
    | _ -> false
  in
  Alcotest.testable pp equal

let test_case_of_problem expected prob =
  let repr = Print.to_string @@ Problem.pp prob in
  Alcotest.test_case
    repr
    `Quick
    (fun () ->
      Alcotest.check result repr expected (Problem.to_solution prob))

let x_id = Term.V.fresh ~name:"x" ()
let y_id = Term.V.fresh ~name:"y" ()
let z_id = Term.V.fresh ~name:"z" ()
let x, y, z = Term.(Var x_id, Var y_id, Var z_id)

let test_valid () =
  let open Term in
  let open Problem in
  List.map
    (test_case_of_problem `Valid)
    [
      (* Congruence-like tests. *)
      Eq (x, x);
      Eq (Comp (x, y), Comp (x, y));
      Eq (Over (x, y), Over (x, y));
      Eq (Under (x, y), Under (x, y));
      Eq (Meet (x, y), Meet (x, y));
      Eq (Join (x, y), Join (x, y));

      (* Universal properties. *)
      Le (Comp (x, Under (x, y)), y);
      Le (Comp (Over (x, y), y), x);
      Le (x, Under (Comp (x, y), y));
      Le (y, Under (x, Comp (x, y)));
      Le (Bot, x);
      Le (Meet (x, y), y);
      Le (Meet (x, y), x);
      Le (x, Top);
      Le (x, Join (x, y));
      Le (y, Join (x, y));

      (* Lattice properties. *)
      Eq (Meet (x, Top), x);
      Eq (Meet (x, Meet (y, z)), Meet (Meet (x, y), z));
      Eq (Meet (x, y), Meet (y, x));
      Eq (Meet (x, x), x);
      Eq (Join (x, Bot), x);
      Eq (Join (x, Join (y, z)), Join (Join (x, y), z));
      Eq (Join (x, y), Join (y, x));
      Eq (Join (x, x), x);
    ]

let test_invalid () =
  let open Term in
  let open Problem in
  List.map
    (test_case_of_problem (`Invalid Counterexample.dummy))
    [
      Le (x, y);
      Le (x, Comp (x, x));
      Le (Comp (x, y), Comp (y, x));
    ]

let () =
  let open Alcotest in
  run "TWMC"
    [
      "validity", test_valid ();
      "invalidity", test_invalid ();
    ]

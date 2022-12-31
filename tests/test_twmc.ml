open Twmclib

let result =
  Alcotest.testable
    (Print.PPrint.to_fmt Problem.Solution.pp)
    Problem.Solution.equal

let test_case_of_problem expected pb =
  let repr = Print.PPrint.to_string @@ Problem.pp pb in
  Alcotest.test_case
    repr
    `Quick
    (fun () ->
      let stms = Problem.existence_statements (module Backends.Z3) pb in
      Alcotest.check result repr expected (Problem.solve_with_z3 stms))

let x_id = Term.V.fresh ~name:"x" ()
let y_id = Term.V.fresh ~name:"y" ()
let z_id = Term.V.fresh ~name:"z" ()
let x, y, z = Term.(Var x_id, Var y_id, Var z_id)

let test_valid () =
  let open Term in
  let open Problem in
  List.map
    (test_case_of_problem Problem.Solution.valid)
    [
      (* Congruence-like tests. *)
      Eq (x, x);
      Eq (Comp (x, y), Comp (x, y));
      Eq (Over (x, y), Over (x, y));
      Eq (Under (x, y), Under (x, y));
      Eq (Meet (x, y), Meet (x, y));
      Eq (Join (x, y), Join (x, y));

      (* Simple algebraic laws. *)
      Le (Comp (x, Id), x);
      Le (x, Comp (x, Id));

      (* Universal properties. *)
      Le (Comp (x, Under (x, y)), y);
      Le (Comp (Over (x, y), y), x);
      Le (x, Over (Comp (x, y), y));
      Le (y, Under (x, Comp (x, y)));
      Le (Meet (x, y), y);
      Le (Meet (x, y), x);
      Le (x, Join (x, y));
      Le (y, Join (x, y));

      (* Lattice properties. *)
      Eq (Meet (x, Meet (y, z)), Meet (Meet (x, y), z));
      Eq (Meet (x, y), Meet (y, x));
      Eq (Meet (x, x), x);
      Eq (Join (x, Join (y, z)), Join (Join (x, y), z));
      Eq (Join (x, y), Join (y, x));
      Eq (Join (x, x), x);
    ]

let test_invalid () =
  let open Term in
  let open Problem in
  List.map
    (test_case_of_problem Solution.invalid)
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

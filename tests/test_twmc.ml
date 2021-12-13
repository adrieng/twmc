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

let test_le () =
  let cases =
    let open Term in

    let x_id = V.fresh ~name:"x" () in
    let y_id = V.fresh ~name:"y" () in
    let x = Var x_id in
    let y = Var y_id in

    Problem.[
        Le (x, x), true;
        Le (Bot, x), true;
        Le (x, Top), true;

        (* Congruence-like tests. *)
        Eq (x, x), true;
        Eq (Comp (x, y), Comp (x, y)), true;
        Eq (Over (x, y), Over (x, y)), true;
        Eq (Under (x, y), Under (x, y)), true;
        Eq (Meet (x, y), Meet (x, y)), true;
        Eq (Join (x, y), Join (x, y)), true;

        (* Universal properties. *)
        Le (Comp (x, Under (x, y)), y), true;
        Le (Comp (Over (x, y), y), x), true;
        Le (x, Under (Comp (x, y), y)), true;
        Le (y, Under (x, Comp (x, y))), true;
        Le (Meet (x, y), y), true;
        Le (Meet (x, y), x), true;
        Le (x, Join (x, y)), true;
        Le (y, Join (x, y)), true;

        (* Some trivialy-invalid relations. *)
        Le (x, y), false;
        Le (x, Comp (x, x)), false;
        Le (Comp (x, y), Comp (y, x)), false;
    ]
  in
  List.map
    (fun (prob, is_valid) ->
      let repr = Print.to_string @@ Problem.pp prob in
      Alcotest.test_case
        repr
        `Quick
        (fun () ->
          let expected =
            if is_valid then `Valid else `Invalid Counterexample.dummy
          in
          Alcotest.check result repr expected (Problem.to_solution prob)))
    cases

let () =
  let open Alcotest in
  run "TWMC"
    [
      "validity", test_le ();
    ]

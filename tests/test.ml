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
    let x = Var x_id in

    Problem.[
        Eq (x, x), true;
        Eq (Comp (x, x), Comp (x, x)), true;
    ]
  in
  List.map
    (fun (prob, is_valid) ->
      let repr =
        Printf.sprintf "%s (%svalid)"
          (Print.to_string @@ Problem.pp prob)
          (if is_valid then "" else "in")
      in
      Alcotest.test_case
        repr
        `Quick
        (fun () -> Alcotest.check
                     result
                     repr
                     (Problem.to_solution prob)
                     (if is_valid then `Valid else `Invalid Counterexample.dummy)))
    cases;;

let () =
  let open Alcotest in
  run "TWMC"
    [
      "le", test_le ();
    ]

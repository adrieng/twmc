module SMTLIB = struct
  module V = Symbol.Make()

  type var = V.t

  type term =
    | Int of int
    | Var of V.t
    | Add of term * term

  let lit i =
    Int i

  let var v =
    Var v

  let add x y =
    Add (x, y)

  let ( + ) =
    add

  type prop =
    | Cmp of [`Eq | `Le | `Lt] * term * term
    | Prop of [`And | `Or | `Entails | `Not] * prop list

  let ( = ) f g =
    Cmp (`Eq, f, g)

  let ( <= ) f g =
    Cmp (`Le, f, g)

  let ( < ) f g =
    Cmp (`Lt, f, g)

  let and_ fs =
    Prop (`And, fs)

  let or_ fs =
    Prop (`Or, fs)

  let not_ f =
    Prop (`Not, [f])

  let entails fs g =
    Prop (`Entails, fs @ [g])

  let ( && ) f g =
    and_ [f; g]

  let ( || ) f g =
    or_ [f; g]

  let ( ==> ) f g =
    entails [f] g

  type phrase =
    | Ass of prop
    | Comment of string

  type query =
    {
      mutable contents : phrase list;
      mutable variables : V.t list;
    }

  let make () =
    {
      contents = [];
      variables = [];
    }

  let assert_ prb f =
    prb.contents <- Ass f :: prb.contents

  let comment prb c =
    prb.contents <- Comment c :: prb.contents

  let fresh ?(name = "") prb =
    let x = V.fresh ~name () in
    prb.variables <- x :: prb.variables;
    x

  let pp query =
    let open PPrint in

    let pp_sexp op f args =
      group @@ parens @@ !^ op ^/^ separate_map (break 1) f args
    in

    let rec pp_var x =
      !^ (V.to_string x)

    and pp_term p =
      match p with
      | Int i ->
         !^ (string_of_int i)
      | Var x ->
         pp_var x
      | Add (x, y) ->
         group (!^ "(+ " ^/^ pp_term x ^//^ pp_term y ^/^ !^ ")")

    and pp_prop f =
      match f with
      | Cmp (op, t, u) ->
         let s =
           match op with
           | `Eq -> "="
           | `Le -> "<="
           | `Lt -> "<"
         in
         pp_sexp s pp_term [t; u]
      | Prop (op, fs) ->
         let s =
           match op with
           | `And -> "and"
           | `Or -> "or"
           | `Entails -> "=>"
           | `Not -> "not"
         in
         pp_sexp s pp_prop fs

    and pp_phrase ph =
      match ph with
      | Ass f ->
         pp_sexp "assert" pp_prop [f]
      | Comment c ->
         !^ ("; " ^ c)
    in

    let hardlines f xs = concat_map (fun x -> f x ^^ hardline) xs in
    let variables =
      hardlines
        (fun x -> !^ "(declare-const " ^^ pp_var x ^^ !^ " Int)")
        query.variables
    in
    let contents = hardlines pp_phrase (List.rev query.contents) in
    concat
      [
        !^ "(set-logic QF_LIA)\n";
        variables;
        contents;
        !^ "; Infrastructure\n";
        !^ "(check-sat)\n";
        !^ "(get-model)\n";
        !^ "(exit)\n";
      ]

  let to_channel oc query =
    PPrint.ToChannel.pretty 0.9 80 oc (pp query)
end

module Z3 = struct
  let new_context () =
    Z3.mk_context [("model", "true")]

  let cx =
    ref (new_context ())

  let flush () =
    cx := new_context ()

  module V : Sigs.HashedOrderedType with type t = Z3.AST.ast = struct
    include Z3.AST
    type t = ast
  end

  type var = V.t

  type term = Z3.Expr.expr

  let lit i =
    Z3.Expr.mk_numeral_int !cx i (Z3.Arithmetic.Integer.mk_sort !cx)

  let var v =
    Z3.Expr.expr_of_ast v

  let add x y =
    Z3.Arithmetic.mk_add !cx [x; y]

  let ( + ) =
    add

  type prop = Z3.Expr.expr

  let ( = ) t u =
    Z3.Boolean.mk_eq !cx t u

  let ( <= ) t u =
    Z3.Arithmetic.mk_le !cx t u

  let ( < ) t u =
    Z3.Arithmetic.mk_lt !cx t u

  let and_ fs =
    Z3.Boolean.mk_and !cx fs

  let or_ fs =
    Z3.Boolean.mk_or !cx fs

  let not_ f =
    Z3.Boolean.mk_not !cx f

  let entails fs g =
    Z3.Boolean.mk_implies !cx (and_ fs) g

  let ( && ) f g =
    and_ [f; g]

  let ( || ) f g =
    or_ [f; g]

  let ( ==> ) f g =
    Z3.Boolean.mk_implies !cx f g

  type query =
    {
      mutable variables : var list;
      solver : Z3.Solver.solver;
    }

  let make () =
    {
      variables = [];
      solver = Z3.Solver.mk_solver_s !cx "QF_LIA";
    }

  let fresh ?(name = "") query =
    let int_s = Z3.Arithmetic.Integer.mk_sort !cx in
    let x = Z3.Expr.(ast_of_expr @@ mk_fresh_const !cx name int_s) in
    query.variables <- x :: query.variables;
    x

  let assert_ query f =
    Z3.Solver.add query.solver [f]

  let comment _ s =
    Z3.Log.append s

  let () =
    ignore @@ Z3.Log.open_ "/tmp/z3.log"

  let pp query =
    PPrint.(!^ (Z3.Solver.to_string query.solver))

  let solve query =
    Z3.Solver.check query.solver []

  let model query =
    match Z3.Solver.get_model query.solver with
    | None ->
       None
    | Some m ->
       let eval x =
         match Z3.Model.get_const_interp_e m (Z3.Expr.expr_of_ast x) with
         | None ->
            failwith ("unknown variable " ^ Z3.AST.to_string x)
         | Some e ->
            if not (Z3.Expr.is_numeral e)
            then failwith ("ill-typed result for" ^ Z3.AST.to_string x);
            Z.to_int @@ Z3.Arithmetic.Integer.get_big_int e
       in
       Some eval
end

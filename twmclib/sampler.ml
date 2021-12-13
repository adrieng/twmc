module Make(X : Sigs.HashedOrderedType)(L : Logic.S) = struct
  module HX = Hashtbl.Make(X)
  module HP = Hashtbl.Make(L.V)
  module SP = Set.Make(L.V)

  type t =
    {
      prb : L.query;
      tab : L.V.t HP.t HX.t;
      mutable all : SP.t;
    }

  let make prb =
    {
      prb;
      tab = HX.create 100;
      all = SP.empty;
    }

  let fresh_point ?name s =
    let p = L.fresh ?name s.prb in
    s.all <- SP.add p s.all;
    p

  let find_or_create find add make x p =
    try find p x with Not_found -> let y = make () in add p x y; y

  let sample s x p =
    let new_sample () = fresh_point ~name:"p" s in
    let s_x = find_or_create HX.find HX.add (fun () -> HP.create 10) x s.tab in
    find_or_create HP.find HP.add new_sample p s_x

  let iter_samples f s =
    HX.iter (fun x s_x -> HP.iter (fun p q -> f x p q) s_x) s.tab

  let iter_sample_pairs f s =
    iter_samples
      (fun x p1 q1 ->
        HP.iter (fun p2 q2 ->
            if not (L.V.equal p1 p2) then f x p1 q1 p2 q2) (HX.find s.tab x))
      s

  let iter_points f s =
    SP.iter f s.all

  let fold_samples f s acc =
    HX.fold
      (fun x s_x acc -> HP.fold (fun p q acc -> f x p q acc) s_x acc)
      s.tab
      acc

  let fold_points f s acc =
    SP.fold f s.all acc
end

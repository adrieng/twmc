type elem = int

type t =
  {
    desc : desc;
    length : int;
    weight : elem;
  }

and desc =
  | Single of elem
  | Concat of t list
  | Power of t * int

let rec print_short fmt w =
  let open Print.Fmt in
  match w.desc with
  | Single i ->
     Format.fprintf fmt "%d" i
  | Concat w_l ->
     pp_list print_short fmt w_l
  | Power ({ desc = Single i; _ }, j) ->
     Format.fprintf fmt "%d^%d"
       i
       j
  | Power (w, j) ->
     Format.fprintf fmt "{ %a }^%d"
       print_short w
       j

let rec _print_full fmt w =
  let open Print.Fmt in
  match w.desc with
  | Single i ->
     Format.fprintf fmt "%d" i
  | Concat w_l ->
     Format.fprintf fmt "{ @[%a@] }"
       (pp_list _print_full) w_l
  | Power (w, j) ->
     Format.fprintf fmt "%a^%d"
       _print_full w
       j

let fmt =
  print_short

let empty =
  {
    desc = Concat [];
    length = 0;
    weight = 0;
  }

let singleton m =
  {
    desc = Single m;
    length = 1;
    weight = m;
  }

let concat w_l =
  let rec simplify racc w_l =
    match w_l with
    | [] ->
       List.rev racc
    | w :: w_l ->
       if w.length = 0
       then simplify racc w_l
       else simplify (w :: racc) w_l
  in
  let w_l = simplify [] w_l in
  match w_l with
  | [] ->
     empty
  | [w] ->
     w
  | _ :: _ ->
     {
       desc = Concat w_l;
       length = List.fold_left (fun n t -> n + t.length) 0 w_l;
       weight = List.fold_left (fun m t -> m + t.weight) 0 w_l;
     }

let (^^) w1 w2 =
  concat [w1; w2]

let power w n =
  if n = 0 then empty
  else if n = 1 then w
  else
    {
      desc = Power (w, n);
      length = w.length * n;
      weight = w.weight * n;
    }

let of_list l =
  List.fold_left (fun w i -> w ^^ singleton i) empty l

let rec split_at n w =
  assert (n >= 0 && n <= w.length);
  if n = 0 then (empty, w)
  else if n > w.length then invalid_arg "split_at: word too short"
  else match w.desc with
       | Single _ ->
          if n = 0 then empty, w else w, empty
       | Concat w_l ->
          let rec find rpref n w_l =
            match w_l with
            | [] ->
               rpref, w_l
            | w :: w_l ->
               if n < w.length
               then
                 let w', w'' = split_at n w in
                 (w' :: rpref, w'' :: w_l)
               else find (w :: rpref) (n - w.length) w_l
          in
          let rpref, rest = find [] n w_l in
          concat (List.rev rpref), concat rest
       | Power (w, i) ->
          let n' = n mod w.length in
          let i' = n / w.length in
          let w', w'' = split_at n' w in
          concat [power w i'; w'], concat [w''; power w (i - i' - 1)]

let drop n w =
  snd (split_at n w)

let rotate w =
  let w1, w2 = split_at 1 w in
  w2 ^^ w1

let rec rev w =
  let wd =
    match w.desc with
    | Single _ ->
       w.desc
    | Concat w_l ->
       Concat (List.rev_map rev w_l)
    | Power (w, n) ->
       Power (rev w, n)
  in
  { w with desc = wd; }

let rec at w i =
  assert (i >= 0 && i < w.length);
  match w.desc with
  | Single m ->
     m
  | Concat w_l ->
     at_l w_l i
  | Power (w, _) ->
     at w (i mod w.length)

and at_l w_l i =
  (* assert (i >= 0 && i < List.length w_l); *)
  match w_l with
  | [] ->
     assert false
  | w :: w_l ->
     if i < w.length then at w i else at_l w_l (i - w.length)

let length w =
  w.length

let weight w =
  w.weight

let is_empty w =
  w.length = 0

let has_null_weight w =
  w.weight = 0

let rec all_equal m w =
  w.length = 0 ||
    match w.desc with
    | Single m' ->
       m = m'
    | Concat w_l ->
       List.for_all (all_equal m) w_l
    | Power (w, _) ->
       all_equal m w

let equal w1 w2 =
  let n = length w1 in

  let rec loop i =
    (i >= n) || (at w1 i = at w2 i && loop (i + 1))
  in
  length w1 = length w2 && loop 0

let compare w1 w2 =
  if w1 == w2 then 0
  else if equal w1 w2 then 0
  else
    let rec compare_desc i =
      assert (i >= 0 && i <= w1.length);
      if i = w1.length then 0
      else
        Sigs.Compare.andthen Sigs.Compare.int
          (lazy (compare_desc (i + 1)))
          (at w1 i) (at w2 i)
    in
    Sigs.Compare.(andthen (pair int int)
                    (lazy (compare_desc 0))
                    (w1.length, w1.weight) (w2.length, w2.weight))

let to_seq w =
  let rec desc d k : int Seq.t =
    match d with
    | Single n ->
       fun () -> Seq.Cons (n, k)
    | Concat w_l ->
       desc_list w_l k
    | Power (w, n) ->
       power w n k

  and desc_list w_l k : int Seq.t =
    match w_l with
    | [] -> k
    | w :: w_l -> desc w.desc (desc_list w_l k)

  and power w n k : int Seq.t =
    if n = 0 then k else desc w.desc (power w (n - 1) k)

  in

  desc w.desc (fun () -> Seq.Nil)

exception Found

let find_first_non_null_index w =
  let r = ref 0 in
  if w.weight = 0 then invalid_arg "find_first_non_null_index";
  try
    Seq.iter
      (fun i -> (if i > 0 then raise Found); incr r)
      (to_seq w);
    assert false;
  with Found -> !r

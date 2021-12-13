(* -*- mode: tuareg -*- *)
%{
  open Twmclib.Warp

  let var =
    let module H = Hashtbl.Make(Twmclib.Sigs.String) in
    let ht = H.create 10 in
    fun name ->
    match H.find ht name with
    | y ->
       y
    | exception Not_found ->
       let y = V.fresh ~name () in
       H.add ht name y;
       y
%}

%start<Twmclib.Warp.rel> problem

(* Tokens *)

%token BOT TOP STAR
%token LPAREN RPAREN
%token<string> ID
%token LE
%token EOF

(* Priorities *)

%left STAR

%%

(* Utilities *)

%inline paren(X):
| LPAREN x = X RPAREN { x }

var:
| x = ID { var x }

simple_atom:
| x = var { Var x }
| TOP { Top }
| BOT { Bot }
| LPAREN t = atom RPAREN { t }

atom:
| t = simple_atom { t }
| t = atom STAR u = atom { Star (t, u) }

rel:
| t = atom LE u = atom { Le (t, u) }

problem:
| r = rel EOF { r }

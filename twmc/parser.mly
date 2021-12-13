(* -*- mode: tuareg -*- *)
%{
  open Twmclib.Term

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

%start<Twmclib.Problem.t> problem

(* Tokens *)

%token<string> IDENT

%token BOT TOP ID
%token STAR SLASH ASLASH MEET JOIN
%token EO ER EL
%token LPAREN RPAREN

%token LE EQ

%token EOF

(* Priorities *)

%left STAR
%right SLASH ASLASH
%left MEET JOIN

%%

(* Utilities *)

%inline paren(X):
| LPAREN x = X RPAREN { x }

var:
| x = IDENT { var x }

simple_term:
| x = var { Var x }
| ID { Id }
| TOP { Top }
| BOT { Bot }
| LPAREN t = term RPAREN { t }

term:
| t = simple_term { t }
| t = term STAR u = term { Comp (u, t) }
| t = simple_term u = term %prec STAR { Comp (t, u) }
| t = term SLASH u = term { Over (t, u) }
| t = term ASLASH u = term { Under (t, u) }
| t = term MEET u = term { Meet (t, u) }
| t = term JOIN u = term { Join (t, u) }
| t = simple_term EO { RedO t }
| t = simple_term EL { RedL t }
| t = simple_term ER { RedR t }

rel:
| t = term LE u = term { Twmclib.Problem.Le (t, u) }
| t = term EQ u = term { Twmclib.Problem.Eq (t, u) }

problem:
| r = rel EOF { r }

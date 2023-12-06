%{
  open Ast
%}

%token <int> Int
%token <string> Id
%token <string> String
%token Type Of Let Rec In If Then Else Match With Fun True False Mod TInt TBool TString TUnit
%token Eq Plus Minus Times Divide Lt Concat Not Negate And Or DoubleSemicolon Colon Arrow DoubleArrow
%token LParen RParen Pipe Comma EOF

(* Lowest precedence *)
%right Arrow
%nonassoc DoubleArrow
%nonassoc In
%nonassoc Else
%left And Or
%left Eq Lt
%left Plus Minus Concat
%left Times Divide Mod
%left Not Negate
(* Highest precedence *)

%start <Ast.program> start
%type <Ast.program> program
%type <Ast.binding> binding
%type <Ast.expr> expr if_expr let_expr fun_expr match_expr apply_expr primary_expr basic_expr tuple_expr t_expr
%type <Ast.expr list> expr_list_no_empty
%type <Ast.typ> type_
%type <Ast.type_decl> type_decl
%type <Ast.type_decl list> type_decl_list
%type <Ast.match_branch> match_branch
%type <Ast.match_branch list> match_branches_no_empty
%type <Ast.param> param
%type <Ast.param list> param_list
%type <Ast.pattern_vars> pattern_vars
%type <string list> pattern_vars_list
%type <Ast.param list option> option_param_list
%type <Ast.typ option> option_type


%%

start:
  | p = program; EOF; { p }

program:
  | b = binding; DoubleSemicolon; ps = program; { b :: ps }
  | b = binding; DoubleSemicolon; { [b] }

binding:
  | Let; id = Id; ps = option_param_list; o = option_type ; Eq; e = expr; { LetBinding (id, ps, o , e) }
  | Let; Rec; id = Id; ps = option_param_list; o = option_type ; Eq; e = expr; { LetRecBinding (id, ps, o, e) }
  | Type; id = Id; Eq; ts = type_decl_list ; { TypeBinding (id, ts) }

type_decl:
  | Pipe; id = Id; { TypeDecl (id, None) }
  | Pipe; id = Id; Of; t = type_; { TypeDecl (id, Some t) }

type_decl_list:
  | t = type_decl; ts = type_decl_list; { t :: ts }
  | t = type_decl; { [t] }

(* Disambiguation rules *)
expr:
  | or_expr = expr; Or; and_expr = expr; { BinOp (OrBinop, or_expr, and_expr) }
  | and_expr = expr; And; comp_expr = expr; { BinOp (AndBinOp, and_expr, comp_expr) }
  | comp_expr = expr; Eq; add_expr = expr; { BinOp (Equal, comp_expr, add_expr) }
  | comp_expr = expr; Lt; add_expr = expr; { BinOp (LessThan, comp_expr, add_expr) }
  | add_expr = expr; Plus; mul_expr = expr; { BinOp (Add, add_expr, mul_expr) }
  | add_expr = expr; Minus; mul_expr = expr; { BinOp (Sub, add_expr, mul_expr) }
  | add_expr = expr; Concat; mul_expr = expr; { BinOp (Concatenation, add_expr, mul_expr) }
  | mul_expr = expr; Times; unary_expr = expr; { BinOp (Mul, mul_expr, unary_expr) }
  | mul_expr = expr; Divide; unary_expr = expr; { BinOp (Div, mul_expr, unary_expr) }
  | mul_expr = expr; Mod; unary_expr = expr; { BinOp (Modulo, mul_expr, unary_expr) }
  | Not; unary_expr = expr; { UnaryOp (NotUnOp, unary_expr) }
  | Negate; unary_expr = expr; { UnaryOp (NegateUnOp, unary_expr) }
  | t_expt = t_expr; { t_expt }

if_expr:
  | If; e1 = expr; Then; e2 = expr; Else; e3 = expr; { IfThenElse (e1, e2, e3) }

let_expr:
  | Let; id = Id; ps = option_param_list; o = option_type ; Eq; e1 = expr; In; e2 = expr; { LetFun (id, ps, o, e1, e2) }
  | Let; Rec; id = Id; ps = option_param_list; o = option_type ; Eq; e1 = expr; In; e2 = expr; { LetRec (id, ps, o, e1, e2) }

fun_expr:
  | Fun; ps = param_list; o = option_type ; DoubleArrow ; e = expr; { Function (ps, o, e) }

match_expr:
  | Match; e = expr; With; bs = match_branches_no_empty; { MatchWith (e, bs) }

t_expr:
  | if_expr = if_expr; { if_expr }
  | let_expr = let_expr; { let_expr }
  | fun_expr = fun_expr; { fun_expr }
  | match_expr = match_expr; { match_expr }
  | apply_expr = apply_expr; { apply_expr }

apply_expr:
  | apply_expr = apply_expr; primary_expr = primary_expr; { FunCall (apply_expr, primary_expr) }
  | primary_expr = primary_expr; { primary_expr }

primary_expr:
  | LParen; expr = expr; RParen; { expr }
  | basic_expr = basic_expr; { basic_expr }
  | tuple_expr = tuple_expr; { tuple_expr }

tuple_expr:
  | LParen; expr = expr; exprs = expr_list_no_empty; RParen; { TupleExpr (expr, exprs) }

basic_expr:
  | i = Int; { IntLiteral i }
  | True; { BoolLiteral true }
  | False; { BoolLiteral false }
  | s = String; { StringLiteral s }
  | id = Id; { Identifier id }
  | LParen; RParen; { Unit }

option_type:
  | Colon; t = type_; { Some t }
  | { None }

expr_list_no_empty:
  | Comma; e = expr; es = expr_list_no_empty; { e :: es }
  | Comma; e = expr; { [e] }

match_branch:
  | Pipe; id = Id; ps = pattern_vars; DoubleArrow ; e = expr; { Branch (id, Some ps, e) }
  | Pipe; id = Id; DoubleArrow ; e = expr; { Branch (id, None, e) }

match_branches_no_empty:
  | b = match_branch; bs = match_branches_no_empty; { b :: bs }
  | b = match_branch; { [b] }

pattern_vars:
  | id = Id; { SingleVar id }
  | LParen; id = Id; ps = pattern_vars_list; RParen; { TupleVar (id, ps) }

pattern_vars_list:
  | Comma; id = Id; ps = pattern_vars_list; { id :: ps }
  | Comma; id = Id; { [id] }

param:
  | id = Id; { SimpleParam id }
  | LParen; id = Id; Colon; t = type_; RParen; { TypedParam (id, t) }

param_list:
  | ps = param; ps2 = param_list; { ps :: ps2 }
  | p = param; { [p] }

option_param_list:
  | ps = param_list; { Some ps }
  | { None }

type_:
  | t1 = type_; Arrow; t2 = type_; { FuncType (t1, t2) }
  | LParen; t = type_; RParen; { SingleType t }
  | t = type_; Times; t2 = type_; { TupleType (t, t2) }
  | TInt; { IntType }
  | TBool; { BoolType }
  | TString; { StringType }
  | TUnit; { UnitType }
  | id = Id; { CustomType id }

%%
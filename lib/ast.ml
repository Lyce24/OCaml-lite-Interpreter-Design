type identifier = string

(* Types *)
type typ =
  | FuncType of typ * typ
  | SingleType of typ
  | TupleType of typ * typ
  | IntType
  | BoolType
  | StringType
  | UnitType
  | CustomType of identifier

(* Expressions *)
type expr =
  | LetFun of identifier * param list option * typ option * expr * expr (* let $id [<param>]* [: <type>] = <expr> in <expr> *)
  | LetRec of identifier * param list option * typ option * expr * expr (* let rec $id [<param>]* [: <type>] = <expr> in <expr> *)
  | IfThenElse of expr * expr * expr (* if <expr> then <expr> else <expr> *)
  | Function of param list * typ option * expr (* fun [<param>]+ [: <type>] => <expr> *)
  | FunCall of expr * expr (* <expr> <expr> *)
  | TupleExpr of expr * expr list (* ( <expr> [, <expr>]+ ) *)
  | BinOp of binop * expr * expr (* <expr> <binop> <expr> *)
  | UnaryOp of unaryop * expr (* <unop> <expr> *)
  | IntLiteral of int
  | BoolLiteral of bool
  | StringLiteral of string
  | Identifier of identifier
  | Unit
  | MatchWith of expr * match_branch list (* match <expr> with ['|' <match_branch>]+ *)

(* Binary and unary operators *)
and binop =
  | Add
  | Sub
  | Mul
  | Div
  | Modulo
  | LessThan
  | Equal
  | Concatenation
  | AndBinOp
  | OrBinop

and unaryop = NotUnOp | NegateUnOp

(* Parameters for functions *)
and param =
  | SimpleParam of identifier
  | TypedParam of identifier * typ

(* Match branches *)
and match_branch =
  | Branch of identifier * pattern_vars option * expr

and pattern_vars =
  | SingleVar of identifier
  | TupleVar of identifier * identifier list

(* Top-level bindings *)
type binding =
  | LetBinding of identifier * param list option * typ option * expr (* let $id [<param>]* [: <type>] = <expr> *)
  | LetRecBinding of identifier * param list option * typ option * expr (* let rec $id [<param>]* [: <type>] = <expr> *)
  | TypeBinding of identifier * type_decl list

and type_decl =
  | TypeDecl of identifier * typ option

type program = binding list
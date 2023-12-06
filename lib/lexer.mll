{

open Parser

(** A new kind of error to be thrown when lexing fails. *)
exception SyntaxError of string

let sb = Buffer.create 128

}

(** Identifiers start with a letter or underscore than have any number of
    letters, underscores, or digits. *)
let id = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*

let whitespace = [' ' '\t' '\r' '\n']+
let int = ['0'-'9']+

rule tok = parse
| whitespace { tok lexbuf }
| int as i  { Int (int_of_string i) }
| "type"    { Type }
| "of"      { Of }
| "let"     { Let }
| "rec"     { Rec}
| "in"      { In }
| "if"      { If }
| "then"    { Then }
| "else"    { Else }
| "match"   { Match }
| "with"    { With }
| "fun"     { Fun }
| "true"    { True }
| "false"   { False }
| "mod"     { Mod }
| "int"     { TInt }
| "bool"    { TBool }
| "string"  { TString }
| "unit"    { TUnit }
| '='       { Eq }
| '+'       { Plus }
| '-'       { Minus }
| '*'       { Times }
| '/'       { Divide }
| '<'       { Lt }
| '^'       { Concat }
| "not"     { Not }
| '~'       { Negate }
| "&&"      { And }
| "||"      { Or }
| ";;"      { DoubleSemicolon }
| ':'       { Colon }
| "->"      { Arrow }
| "=>"      { DoubleArrow }
| '('       { LParen }
| ')'       { RParen }
| '|'       { Pipe }
| ','       { Comma }
| '"'       { Buffer.clear sb;
            string lexbuf;
            String (Buffer.contents sb) }
| "(*"      { comment lexbuf }
| id as s   { Id s }
| eof       { EOF }
| _         { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and string = parse
| '"'       { () }
| _ as c    { Buffer.add_char sb c;
            string lexbuf }
and comment = parse
| "*)"      { tok lexbuf }
| _         { comment lexbuf }

{

(* let tok_to_str : token -> string = function
    | Type -> "type" 
    | Of -> "of"
    | Let -> "let"
    | Rec -> "rec"
    | In -> "in"
    | If -> "if"
    | Then -> "then"
    | Else -> "else"
    | Match -> "match"
    | With -> "with"
    | Fun -> "fun"
    | True -> "true"
    | False -> "false"
    | Mod -> "mod"
    | TInt -> "int"
    | TBool -> "bool"
    | TString -> "string"
    | TUnit -> "unit"
    | Eq -> "="
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Divide -> "/"
    | Lt -> "<"
    | Concat -> "^"
    | Not -> "not"
    | Negate -> "~"
    | And -> "&&"
    | Or -> "||"
    | DoubleSemicolon -> ";;"
    | Colon -> ":"
    | Arrow -> "->"
    | DoubleArrow -> "=>"
    | LParen -> "("
    | RParen -> ")"
    | Pipe -> "|"
    | Comma -> ","
    | Int i -> string_of_int i
    | Id s -> s
    | String s -> "\"" ^ s ^ "\""
    | EOF -> "<eof>" *)


let tokenize (s : string) : token list =
  let buf = Lexing.from_string s in
  let rec helper acc =
    match tok buf with
    | EOF -> List.rev acc
    | t -> helper (t :: acc) in
  helper []


let parse_lexbuf (buf : Lexing.lexbuf) : Ast.program = 
    start tok buf

let parse s = parse_lexbuf (Lexing.from_string s)
}

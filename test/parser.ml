open OUnit2
open Ocaml_lite.Lexer
open Ocaml_lite.Ast

let parse_test name input expected_output = 
  name >:: (fun _ -> 
    assert_equal expected_output (parse input))

let parse_tests = "test suite for parse" >::: [
    parse_test "let x = 3" 
    "let x = 3;;"
    [LetBinding("x", None , None, IntLiteral(3))];

    parse_test "let f y = y + 1"
    "let f y = y + 1;;"
    [LetBinding("f", Some [SimpleParam("y")], None, BinOp(Add, Identifier("y"), IntLiteral(1)))];

    parse_test "factorial"
    "let rec factorial n = 
      if n = 0 
        then 1 
      else n * factorial (n - 1);;"
      [LetRecBinding ("factorial", Some [SimpleParam "n"], None,
      IfThenElse (BinOp (Equal, Identifier "n", IntLiteral 0), IntLiteral 1,
      BinOp (Mul, Identifier "n",
      FunCall (Identifier "factorial",
      BinOp (Sub, Identifier "n", IntLiteral 1)))))];
    
    parse_test "let _ = ()"
    "let _ = ();;"
    [LetBinding("_", None , None, Unit)];

    parse_test "let f x y = x + y"
    "let f x y = x + y;;"
    [LetBinding("f", Some [SimpleParam("x"); SimpleParam("y")], None, BinOp(Add, Identifier("x"), Identifier("y")))];

    parse_test "match statement"
    "let match_statement x = match x with
    | NIL => NIL
    | Cons (x, xs) => x + xs;;"
    [LetBinding ("match_statement",
    Some [SimpleParam "x"], None,
    MatchWith (Identifier "x",
     [Branch ("NIL", None,
       Identifier "NIL");
      Branch ("Cons",
       Some (TupleVar ("x", ["xs"])),
       BinOp (Add, Identifier "x",
        Identifier "xs"))]))];

    parse_test "type int_list = | Nil | Cons of int * int_list"
    "type int_list =
    | Nil
    | Cons of int * int_list;;"
    [TypeBinding ("int_list",
    [TypeDecl ("Nil", None);
     TypeDecl ("Cons",
      Some
       (TupleType (IntType,
         CustomType "int_list")))])];

    parse_test "let add_one (x: int) = x + 1"
    "let add_one (x: int) = x + 1;;"
    [LetBinding ("add_one",
    Some [TypedParam ("x", IntType)], None,
    BinOp (Add, Identifier "x",
     IntLiteral 1))];

    parse_test "func call"
    "let _ = print_string (int_to_string x);;"
    [LetBinding ("_", None, None,
      FunCall (Identifier "print_string",
      FunCall (Identifier "int_to_string", Identifier "x")))];

    parse_test "Rec + Match"
    "let rec remove_duplicates l =
      match l with
      | Nil => Nil
      | Cons (x, xs) =>
        if member x xs
          then remove_duplicates xs
        else Cons (x, remove_duplicates xs);;"
    [LetRecBinding ("remove_duplicates",
        Some [SimpleParam "l"], None,
        MatchWith (Identifier "l",
         [Branch ("Nil", None, Identifier "Nil");
          Branch ("Cons",
           Some (TupleVar ("x", ["xs"])),
           IfThenElse
            (FunCall
              (FunCall (Identifier "member",
                Identifier "x"),
              Identifier "xs"),
            FunCall (Identifier "remove_duplicates",
             Identifier "xs"),
            FunCall (Identifier "Cons",
             TupleExpr (Identifier "x",
              [FunCall (Identifier "remove_duplicates",
                Identifier "xs")]))))]))];

    parse_test "in statement"
    "let rec remove_duplicates lst =
      let rec remove_all (x : int) (l : int_list) : int_list =
        match l with
        | Nil => Nil
        | Cons (h, t) => if x = h then remove_all x t else Cons (h, remove_all x t)
      in
      match lst with
      | Nil => Nil
      | Cons (h, t) => Cons (h, remove_duplicates (remove_all h t)) ;;"
      [LetRecBinding ("remove_duplicates", Some [SimpleParam "lst"], None,
      LetRec ("remove_all",
      Some [TypedParam ("x", IntType); TypedParam ("l", CustomType "int_list")],
      Some (CustomType "int_list"),
      MatchWith (Identifier "l",
      [Branch ("Nil", None, Identifier "Nil");
      Branch ("Cons", Some (TupleVar ("h", ["t"])),
      IfThenElse (BinOp (Equal, Identifier "x", Identifier "h"),
      FunCall (FunCall (Identifier "remove_all", Identifier "x"),
      Identifier "t"),
      FunCall (Identifier "Cons",
      TupleExpr (Identifier "h",
      [FunCall (FunCall (Identifier "remove_all", Identifier "x"),
       Identifier "t")]))))]),
      MatchWith (Identifier "lst",
      [Branch ("Nil", None, Identifier "Nil");
      Branch ("Cons", Some (TupleVar ("h", ["t"])),
      FunCall (Identifier "Cons",
      TupleExpr (Identifier "h",
      [FunCall (Identifier "remove_duplicates",
      FunCall (FunCall (Identifier "remove_all", Identifier "h"),
       Identifier "t"))])))])))];

    parse_test "let _ = let x = 3 in x + 1"
    "let _ = let x = 3 in x + 1;;"
    [LetBinding ("_", None, None,
      LetFun ("x", None, None, IntLiteral 3,
      BinOp (Add, Identifier "x",
       IntLiteral 1)))];
  ]

open OUnit2
open Ocaml_lite.Interpreter
open Ocaml_lite.Lexer

let interpret_test name input expected_output = 
  name >:: (fun _ -> 
    assert_equal expected_output (interpret_program input))

let interpret_tests = "test suite for interpreting" >::: [
    interpret_test "let x = 3" 
    (parse("let x = 3;;"))
    ();

    interpret_test "Basic addition"
    (parse("let _ = let f x = x + 2 in f 3;;"))
    ();

    interpret_test "Basic subtraction"
    (parse("let _ = let f x = x - 2 in f 3;;"))
    ();

    interpret_test "Recursive Function"
    (parse("let _ = let rec f x = if x = 0 then 1 else x * f (x - 1) in f 5;;"))
    ();

    interpret_test "Basic Function Definition"
    (parse("let f x y = fun x y => x + y;;"))
    ();

    interpret_test "Basic Type Declaration"
    (parse("type Test = | A | B of int | C of int * int |D of int * Test;;"))
    ();
    
    interpret_test "Match Statement"
    (parse("type list = | Nil | Cons of int * list;;
      let rec sum lst = match lst with
      | Nil => 0
      | Cons (h, t) => h + sum t ;;"))
    ();

    interpret_test "Comprehensive Test"
    (parse("type list = | Nil | Cons of int * list;;
            let _ = let f x = x + 1 in f 1;; 
            let f = fun x => x + 1;; 
            let test = let f x y = x + y in f 1 2;; 
            let f x y = x + y;;
            let rec sum lst = match lst with
            | Nil => 0
            | Cons (h, t) => h + sum t ;;
            let test_list = sum (Cons (1, Cons (2, Cons (3, Nil))));;"))
    ();

  ]

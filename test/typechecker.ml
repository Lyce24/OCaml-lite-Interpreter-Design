(*

1. Input: let x: int = 3;;
Expected Output: true

2. Input: let y: string = 5;;
Expected Output: false

3. Input: let _ = 3 ^ 'x';;
Expected Output: false

4. Input: let add (x: int) (y: int): int = x + y;;
Expected Output: true

5. Input: let z: bool = true || false;;
Expected Output: true

6. Input: let f: int -> int = fun x => x * 2;;
Expected Output: true

7. Input: let g: string -> int = fun s => s + 1;;
Expected Output: false

8. Input: type t = | A | B of int in let v: t = B "hello";;
Expected Output: false

9. Input: match 3 with | x => x * 2;;
Expected Output: true

10. Input: let _ = if true then 3 else "hello";;
Expected Output: false

11. Input: 
  type int_list =
  | Nil
  | Cons of int * int_list ;;

  Excepted Output: true

12. Input: 
  let rec remove_duplicates lst =
    let rec remove_all (x : int) (l : int_list) : int_list =
      match l with
      | Nil => Nil
      | Cons (h, t) => if x = h then remove_all x t else Cons (h, remove_all x t)
  in
  match lst with
  | Nil => Nil
  | Cons (h, t) => Cons (h, remove_duplicates (remove_all h t)) ;;

  Expected Output: true

*)

open OUnit2
open Ocaml_lite.Lexer
open Ocaml_lite.Typechecker

let typecheck_test name input expected_output = 
  name >:: (fun _ -> 
    assert_equal (typecheck_program((parse input))) expected_output )

let typecheck_tests = "test suite for parse" >::: [
    typecheck_test "let x = 3" 
    "let x = 3;;"
    ();

    typecheck_test "let add_one (x: int) = x + 1"
    "let add_one (x: int) = x + 1;;"
    ();

    typecheck_test "let _ = let f x y = x + y in f 3 2"
    "let _ = let f x y = x + y in f 3 2;;"
    ();
    
  ]

(* type A = 
    | Test of int * int * int * int;;


let test = let f x = match x with
                    | Test (a, b, c, d) => a + b + c + d
                    | _ => 1 in f (Test (1, 2, 3, 4));; *)


let id x = x;;

let _ = (id 2, id ());;

let test = let f x = x + 1 in f 1;;

let rec f x = if f (x - 1) < 0 then true else false;;

let add x y =  x + y;;

let result = add 4 9;;
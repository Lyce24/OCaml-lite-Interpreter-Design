open Ast

type value = VInt of int 
            | VBool of bool
            | VString of string
            | VUnit
            | VTuple of value list
            | VClosure of string * (string * value) list * expr * string option
            | VUser of string * value list
            | VConstructor of string

type env = (string * value) list

let rec string_of_expr (e : expr) : string = 
    match e with
    | IntLiteral i -> string_of_int i
    | BoolLiteral b -> string_of_bool b
    | StringLiteral s -> s
    | Unit -> "()"
    | Identifier id -> id
    | IfThenElse (cond, thn, els) -> 
        "if " ^ string_of_expr cond ^ " then " ^ string_of_expr thn ^ " else " ^ string_of_expr els
    | BinOp (op, expr1, expr2) ->
        let op_str = match op with
        | Add -> "+"
        | Sub -> "-"
        | Mul -> "*"
        | Div -> "/"
        | Modulo -> "%"
        | Equal -> "="
        | LessThan -> "<"
        | Concatenation -> "^"
        | AndBinOp -> "&&"
        | OrBinop -> "||"
        in
        "(" ^ string_of_expr expr1 ^ " " ^ op_str ^ " " ^ string_of_expr expr2 ^ ")"
    | UnaryOp (op, expr) ->
        let op_str = match op with
        | NotUnOp -> "not"
        | NegateUnOp -> "-"
        in
        "(" ^ op_str ^ " " ^ string_of_expr expr ^ ")"
    | TupleExpr (expr, exprs) -> 
        "(" ^ string_of_expr expr ^ ", " ^ String.concat ", " (List.map string_of_expr exprs) ^ ")"
    | Function (params, _ , body) ->
        let params_str = match params with
        | [] -> failwith "Function must have at least one parameter"
        | _ -> 
            let rec helper (params : param list) : string =
                match params with
                | [] -> ""
                | param :: rest_params -> 
                    match param with
                    | SimpleParam id | TypedParam (id, _) -> id ^ " " ^ helper rest_params
            in
            helper params
        in
        "fun " ^ params_str ^ " => " ^ string_of_expr body
    | FunCall (func, arg_expr) -> 
        "(" ^ string_of_expr func ^ " " ^ string_of_expr arg_expr ^ ")"
    | LetFun (id, params, _ , body, expr) ->
        let params_str = match params with
        | None -> ""
        | Some params -> 
            match params with
            | [] -> failwith "Function must have at least one parameter"
            | _ -> 
                let rec helper (params : param list) : string =
                    match params with
                    | [] -> ""
                    | param :: rest_params -> 
                        match param with
                        | SimpleParam id | TypedParam (id, _) -> id ^ " " ^ helper rest_params
                in
                helper params
        in
        "let rec " ^ id ^ " " ^ params_str ^ " = " ^ string_of_expr body ^ " in " ^ string_of_expr expr
    | LetRec (func_id, params, _ , body, expr) ->
        let params_str = match params with
        | None -> ""
        | Some params -> 
            match params with
            | [] -> failwith "Function must have at least one parameter"
            | _ -> 
                let rec helper (params : param list) : string =
                    match params with
                    | [] -> ""
                    | param :: rest_params -> 
                        match param with
                        | SimpleParam id | TypedParam (id, _) -> id ^ " " ^ helper rest_params
                in
                helper params
        in
        "let rec " ^ func_id ^ " " ^ params_str ^ " = " ^ string_of_expr body ^ " in " ^ string_of_expr expr
    | MatchWith(e1, branches) -> let _ = string_of_expr e1 in 
        let rec helper (branches : match_branch list) : string = 
            match branches with 
            | [] -> ""
            | br :: rest_br -> (match br with
                                (*identifier * pattern_vars option * expr*)
                                | Branch(id, _ , br_expr) ->  id ^ " -> " ^ string_of_expr br_expr ^ " | " ^ helper rest_br)
        in
        "match " ^ helper branches ^ " _ -> failwith \"No Branch matches.\""


let rec string_of_value (v : value) : string =
    match v with
    | VInt i -> string_of_int i
    | VBool b -> string_of_bool b
    | VString s -> s
    | VUnit -> "()"
    | VTuple vs -> "(" ^ String.concat ", " (List.map string_of_value vs) ^ ")"
    | VClosure (id, _ , func, id1) -> 
        (match id1 with
        | None -> "<" ^ id ^ ", " ^  string_of_expr func ^ ", " ^ "None" ^ ">"
        | Some id1 -> "<" ^ id ^ ", " ^ string_of_expr func ^ ", " ^ id1 ^ ">")
    | VUser (id, vs) -> id ^ "(" ^ String.concat ", " (List.map string_of_value vs) ^ ")"
    | VConstructor id -> id

let rec print_env (env : env) : unit =
    match env with
    | [] -> ()
    | (id, v) :: rest_env -> print_endline (id ^ " = " ^ string_of_value v); print_env rest_env 

let function_closure (param : param) (param_rest : param list) (body : expr) (env_ctx : env) : value =
    match param_rest with
    | [] -> (match param with
        | SimpleParam id | TypedParam (id, _) -> 
                VClosure (id, env_ctx, body, None))
    | _ -> let nested_func = let rec construct_nested_function (param_rest : param list) (body : expr) : expr =
        match param_rest with
        | [] -> body
        | param :: rest_params -> 
            match param with
            | SimpleParam id | TypedParam (id, _) -> Function ([SimpleParam id], None, construct_nested_function rest_params body)
    in
    construct_nested_function param_rest body in
    match param with
    | SimpleParam id | TypedParam (id, _) -> 
            VClosure (id, env_ctx, nested_func, None)
        
  
let rec eval_expr (expr : expr) (eval_env : env) : value = 
    match expr with
    | IntLiteral i -> VInt i
    | BoolLiteral b -> VBool b
    | StringLiteral s -> VString s
    | Unit -> VUnit
    | Identifier id -> (match List.assoc_opt id eval_env with
        | Some v -> v
        | None -> failwith ("Identifier " ^ id ^ " not found"))
    | IfThenElse (cond, thn, els) -> 
        (match eval_expr cond eval_env with
        | VBool true -> eval_expr thn eval_env
        | VBool false -> eval_expr els eval_env
        | _ -> failwith "Condition must be a boolean")
    | BinOp (op, expr1, expr2) ->
        let v1 = eval_expr expr1 eval_env in
        let v2 = eval_expr expr2 eval_env in
        (match op with
        | Add -> (match v1, v2 with
            | VInt i1, VInt i2 -> VInt (i1 + i2)
            | _ -> failwith "Addition requires two integers")
        | Sub -> (match v1, v2 with
            | VInt i1, VInt i2 -> VInt (i1 - i2)
            | _ -> failwith "Subtraction requires two integers")
        | Mul -> (match v1, v2 with
            | VInt i1, VInt i2 -> VInt (i1 * i2)
            | _ -> failwith "Multiplication requires two integers")
        | Div -> (match v1, v2 with
            | VInt i1, VInt i2 -> VInt (i1 / i2)
            | _ -> failwith "Division requires two integers")
        | Modulo -> (match v1, v2 with
            | VInt i1, VInt i2 -> VInt (i1 mod i2)
            | _ -> failwith "Modulo requires two integers")
        | Equal -> (match v1, v2 with
            | VInt i1, VInt i2 -> VBool (i1 = i2)
            | VBool b1, VBool b2 -> VBool (b1 = b2)
            | VString s1, VString s2 -> VBool (s1 = s2)
            | VUnit, VUnit -> VBool true
            | _ -> VBool false)
        | LessThan -> (match v1, v2 with
            | VInt i1, VInt i2 -> VBool (i1 < i2)
            | _ -> failwith "Less than requires two integers")
        | Concatenation -> (match v1, v2 with
            | VString s1, VString s2 -> VString (s1 ^ s2)
            | _ -> failwith "Concatenation requires two strings")
        | AndBinOp -> (match v1, v2 with
            | VBool b1, VBool b2 -> VBool (b1 && b2)
            | _ -> failwith "And requires two booleans")
        | OrBinop -> (match v1, v2 with
            | VBool b1, VBool b2 -> VBool (b1 || b2)
            | _ -> failwith "Or requires two booleans"))
    | UnaryOp (op, expr) ->
        let v = eval_expr expr eval_env in
        (match op with
        | NotUnOp -> (match v with
            | VBool b -> VBool (not b)
            | _ -> failwith "Not requires a boolean")
        | NegateUnOp -> (match v with
            | VInt i -> VInt (-i)
            | _ -> failwith "Negation requires an integer"))

    | TupleExpr (expr, exprs) -> 
        let v = eval_expr expr eval_env in
        let vs = List.map (fun e -> eval_expr e eval_env) exprs in
        VTuple (v :: vs) 

    | Function (params, return_type , body) ->
        (match params with
        | [] -> failwith "Function must have at least one parameter"
        | param :: rest_params ->
            match param with
            | SimpleParam id | TypedParam (id, _) -> 
                match rest_params with
                | [] -> VClosure (id, eval_env, body, None)
                | _ -> VClosure (id, eval_env, (Function(rest_params, return_type , body)), None))

    | FunCall (func, arg_expr) -> 
        (* expr * expr*)
        let func_val = eval_expr func eval_env in
        let arg_val = eval_expr arg_expr eval_env in
        (match func_val with
        | VClosure (param, env, body, id) -> 
            (match id with
            | None -> let new_env = (param, arg_val) :: env in
            eval_expr body new_env
            | Some id -> let new_env = (param, arg_val) :: env in 
                         let new_env = (id, VClosure (param, env, body, Some id)) :: new_env in
            eval_expr body new_env)
        | VConstructor (id) -> (match arg_val with
                                    | VTuple (vs) -> VUser (id, vs)
                                    | _ ->VUser (id, [arg_val]))
        (* Functions for print_string, string_of_int, int_of_string *)
        | _ -> failwith "Function call requires a function")

    | LetFun (id, params, _ , body, expr) ->
        (match params with
        | None -> 
            let body_expr = eval_expr body eval_env in
            let new_env = (id, body_expr) :: eval_env in
            eval_expr expr new_env
        | Some params ->
            match params with
            | [] -> failwith "Function must have at least one parameter"
            | param :: rest_params -> let func_closure = function_closure param rest_params body eval_env in
            let new_env = (id, func_closure) :: eval_env in
            eval_expr expr new_env)
    
    | LetRec (func_id, params, _ , body, expr) ->
        (match params with
        | None -> 
            let body_expr = eval_expr body eval_env in
            let new_env = (func_id, body_expr) :: eval_env in
            eval_expr expr new_env
        | Some params ->
            let func_closure = match params with
            | [] -> failwith "Function must have at least one parameter"
            | param :: rest_params -> match rest_params with
                | [] -> (match param with
                            | SimpleParam id | TypedParam (id, _) -> 
                                    VClosure (id, eval_env, body, Some func_id))
                | _ -> let nested_func = let rec construct_nested_function (param_rest : param list) (body : expr) : expr =
                            match param_rest with
                            | [] -> body
                            | param :: rest_params -> 
                                match param with
                                | SimpleParam id | TypedParam (id, _) -> Function ([SimpleParam id], None, construct_nested_function rest_params body)
            in
            construct_nested_function rest_params body in
            match param with
                | SimpleParam id | TypedParam (id, _) -> 
                        VClosure (id, eval_env, nested_func, Some func_id) in
            let new_env = (func_id, func_closure) :: eval_env in
            eval_expr expr new_env)
    (* expr * match_branch list *)
    | MatchWith(e1, branches) -> let expr_val = eval_expr e1 eval_env in 
        match branches with 
        | [] -> failwith "Match expression must have at least one branch"
        | _ -> match_helper expr_val branches eval_env
    
and match_helper (expr_val : value) (branches : match_branch list) (env : env) : value = 
    (match branches with
    | [] -> failwith "No Branch matches."
    | br :: rest_br -> (match br with
                        (*identifier * pattern_vars option * expr*)
                        | Branch(id, vars, br_expr) -> (match expr_val with
                                                            | VUser(user_id, value_list) -> 
                                                                if user_id = id
                                                                then let new_env = match_vars vars value_list env in
                                                                                eval_expr br_expr new_env
                                                                else match_helper (expr_val) (rest_br) (env)
                                                            | _ -> failwith "Cannot pattern-match literals")))
(* Function to match pattern variables with values and add them to the environment *)
and match_vars (pattern_vars_opt : pattern_vars option) (value_list : value list) (initial_env : env) : env =
    match pattern_vars_opt with
    | None -> initial_env  (* No pattern variables to match *)
    | Some pattern_vars -> 
        match pattern_vars, value_list with
        | SingleVar var, [val_] -> 
            (* Single variable matched with a single value *)
            (var, val_) :: initial_env
        | TupleVar (var, vars), values -> 
            (* Tuple of variables matched with a list of values *)
            match_tuple_vars (var :: vars) values initial_env
        | _ -> failwith "Pattern variables and values do not match in structure or count"

and match_tuple_vars (vars : identifier list) (values : value list) (env : env) : env =
    match vars, values with
    | [], [] -> env
    | var :: vars_tail, val_ :: vals_tail -> 
        (* Match each variable in the tuple with its corresponding value *)
        let new_binding = (var, val_) in
        let new_env = new_binding :: env in
        match_tuple_vars vars_tail vals_tail new_env
    | _ -> failwith "Number of tuple variables does not match number of values"

let type_to_env (eval_env : env) (type_decl : type_decl) : env =
    match type_decl with
    | TypeDecl (id, type_opt) -> 
        (match type_opt with
        | None -> (id, VUser (id, [])) :: eval_env
        | Some _ -> (id, VConstructor id) :: eval_env
        )

let interpret_binding (binding : binding) (eval_env : env) : env =
    match binding with
    | LetBinding (id, param_list, _, expr) -> 
        (match param_list with
        | None -> 
            let v = eval_expr expr eval_env in
            if List.mem_assoc id eval_env then
                let new_env = List.remove_assoc id eval_env in
                (id, v) :: new_env
            else
                (id, v) :: eval_env
        | Some param_list ->
            (match param_list with
            | [] -> failwith "Function must have at least one parameter"
            | param :: rest_params -> let func_closure = function_closure param rest_params expr eval_env in
            if List.mem_assoc id eval_env then
                let new_env = List.remove_assoc id eval_env in
                (id, func_closure) :: new_env
            else
                (id, func_closure) :: eval_env))
    | LetRecBinding (func_id, param_list, _, expr) ->
        (match param_list with
        | None -> 
            let v = eval_expr expr eval_env in
            if List.mem_assoc func_id eval_env then
                let new_env = List.remove_assoc func_id eval_env in
                (func_id, v) :: new_env
            else
                (func_id, v) :: eval_env
        | Some params ->
            let func_closure = match params with
            | [] -> failwith "Function must have at least one parameter"
            | param :: rest_params -> match rest_params with
                | [] -> (match param with
                            | SimpleParam id | TypedParam (id, _) -> 
                                    VClosure (id, eval_env, expr, Some func_id))
                | _ -> let nested_func = let rec construct_nested_function (param_rest : param list) (body : expr) : expr =
                            match param_rest with
                            | [] -> body
                            | param :: rest_params -> 
                                match param with
                                | SimpleParam id | TypedParam (id, _) -> Function ([SimpleParam id], None, construct_nested_function rest_params body)
            in
            construct_nested_function rest_params expr in
            match param with
                | SimpleParam id | TypedParam (id, _) -> 
                        VClosure (id, eval_env, nested_func, Some func_id) in
            if List.mem_assoc func_id eval_env then
                let new_env = List.remove_assoc func_id eval_env in
                (func_id, func_closure) :: new_env
            else
                (func_id, func_closure) :: eval_env)
    | TypeBinding (_, type_decls) ->
        let new_env = List.fold_left (fun env type_decl -> type_to_env env type_decl) eval_env type_decls in
        new_env

let initial_env : env = [
    "print_string", VClosure ("s", [], Function ([SimpleParam "s"], None, FunCall (Identifier "print_string", Identifier "s")), None);
    "string_of_int", VClosure ("i", [], Function ([SimpleParam "i"], None, FunCall (Identifier "string_of_int", Identifier "i")), None);
    "string_of_bool", VClosure ("b", [], Function ([SimpleParam "b"], None, FunCall (Identifier "string_of_bool", Identifier "b")), None);
]

(* Main entry point *)
let interpret_program (p : program): unit = 
    let rec helper (eval_env : env) (p : program) : unit =
        match p with
        | [] -> print_env eval_env; ()
        | binding :: bindings -> let new_env = interpret_binding binding eval_env in
                                helper new_env bindings 
    in
    helper initial_env p
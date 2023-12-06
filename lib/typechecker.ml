open Ast

(* Type expression definitions *)
type type_expr =
  | IntTy 
  | StringTy 
  | BoolTy 
  | UnitTy
  | FuncTy of type_expr * type_expr 
  | TupleTy of type_expr list 
  | UserTy of string
  | VarTy of string 
  | ForallTy of string * type_expr

(* Utility modules *)
module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

type type_env = type_expr StringMap.t

exception UnificationFailure of string
exception TypeError of string

(* Utility functions *)
let next_var = ref 0

let fresh_var (_ : unit) : string =
  let () = next_var := !next_var + 1 in
  "x$" ^ string_of_int !next_var

let initial_env : type_env =
    StringMap.of_seq (List.to_seq [
      ("int_of_string", FuncTy (StringTy, IntTy));
      ("string_of_int", FuncTy (IntTy, StringTy));
      ("print_string", FuncTy (StringTy, UnitTy));
    ])

let rec convert_type (t: typ) : type_expr =
    match t with
    | IntType -> IntTy
    | BoolType -> BoolTy
    | StringType -> StringTy
    | UnitType -> UnitTy
    | CustomType id -> UserTy id
    | FuncType (t1, t2) -> FuncTy (convert_type t1, convert_type t2)
    | TupleType (t1, t2) -> TupleTy [convert_type t1; convert_type t2]
    | SingleType t -> convert_type t

let rec print_type (ty : type_expr) : unit =
      match ty with
      | IntTy -> print_string "int"
      | StringTy -> print_string "string"
      | BoolTy -> print_string "bool"
      | UnitTy -> print_string "unit"
      | FuncTy (arg, ret) ->
          print_string "(";
          print_type arg;
          print_string " -> ";
          print_type ret;
          print_string ")"
      | TupleTy tys ->
          print_string "(";
          List.iter (fun ty -> print_type ty; print_string " * ") tys;
          print_string ")"
      | UserTy name -> print_string name
      | VarTy var -> print_string var
      | ForallTy (var, ty) -> print_string "Forall "; print_string var; print_string ". "; print_type ty
    
let print_env (env : type_env) : unit =
      let print_binding id ty =
        print_string (id ^ " : ");
        (match ty with
         | IntTy -> print_string "int"
         | StringTy -> print_string "string"
         | BoolTy -> print_string "bool"
         | UnitTy -> print_string "unit"
         | FuncTy (arg, ret) ->
             print_string "(";
             print_type arg;
             print_string " -> ";
             print_type ret;
             print_string ")"
         | TupleTy tys ->
             print_string "(";
             List.iter (fun ty -> print_type ty; print_string " * ") tys;
             print_string ")"
         | UserTy name -> print_string name
         | VarTy var -> print_string var
         | ForallTy (var, ty) -> print_string "Forall "; print_string var; print_string ". "; print_type ty);
        print_newline () in
      StringMap.iter print_binding env

(* Generalization *)
let rec free_vars_in_type ty acc = 
  match ty with
  | VarTy v -> StringSet.add v acc
  | FuncTy (arg, ret) -> free_vars_in_type arg (free_vars_in_type ret acc)
  | TupleTy tys -> List.fold_right free_vars_in_type tys acc
  | ForallTy (v, ty) -> free_vars_in_type ty (StringSet.remove v acc) (* Exclude quantified variable *)
  | _ -> acc

let generalize env ty = 
    let free_vars = free_vars_in_type ty StringSet.empty in
    let env_vars = StringMap.fold (fun _ ty acc -> StringSet.union (free_vars_in_type ty StringSet.empty) acc) env StringSet.empty in
    let gen_vars = StringSet.diff free_vars env_vars in
    StringSet.fold (fun var acc -> ForallTy (var, acc)) gen_vars ty

let rec substitute subst ty = 
      match ty with
      | VarTy v -> (try StringMap.find v subst with Not_found -> ty)
      | FuncTy (arg, ret) -> FuncTy (substitute subst arg, substitute subst ret)
      | TupleTy tys -> TupleTy (List.map (substitute subst) tys)
      | ForallTy (v, ty) -> ForallTy (v, substitute (StringMap.remove v subst) ty)
      | _ -> ty
  
let instantiate (ty: type_expr) : type_expr =
    let rec inst (subst, ty) = match ty with
      | ForallTy (var, ty) ->
          let fresh = VarTy (fresh_var ()) in
          inst (StringMap.add var fresh subst, ty)
      | _ -> substitute subst ty
    in
    inst (StringMap.empty, ty)
  
(* Unification *)
let rec occurs_check var ty = 
  match ty with
  | VarTy v -> v = var
  | FuncTy (arg, ret) -> occurs_check var arg || occurs_check var ret
  | TupleTy tys -> List.exists (occurs_check var) tys
  | ForallTy (_, ty) -> occurs_check var ty
  | _ -> false

let substitute_in_environment env var ty =
      let new_env = StringMap.map (substitute (StringMap.singleton var ty)) env in
      print_endline "Environment after substitution: ";
      print_env new_env;
      new_env

let rec unify (env: type_env ref) (ty1: type_expr) (ty2: type_expr) : unit =
      match (ty1, ty2) with
      | (IntTy, IntTy) | (StringTy, StringTy) | (BoolTy, BoolTy) | (UnitTy, UnitTy) -> ()
      | (FuncTy (arg1, ret1), FuncTy (arg2, ret2)) ->
          unify env arg1 arg2;
          unify env ret1 ret2
      | (TupleTy tys1, TupleTy tys2) ->
          if List.length tys1 <> List.length tys2 then
            raise (UnificationFailure "Tuples have different lengths")
          else
            List.iter2 (unify env) tys1 tys2
      | (UserTy name1, UserTy name2) when name1 = name2 -> ()
      | (VarTy var, ty) | (ty, VarTy var) ->
          if occurs_check var ty then
            raise (UnificationFailure ("Recursive unification " ^ var))
          else
            env := substitute_in_environment !env var ty;
      | _ -> if ty1 <> ty2 then raise (UnificationFailure "Type mismatch in unification")

(* Type inference *)
let rec infer_type (env_ref: type_env ref) (expr: expr) : type_expr =
  match expr with
  | IntLiteral _ -> IntTy
  | BoolLiteral _ -> BoolTy
  | StringLiteral _ -> StringTy
  | Unit -> UnitTy
  | Identifier id ->
      (try StringMap.find id !env_ref
       with Not_found -> raise (TypeError ("Unbound identifier: " ^ id)))
  | UnaryOp (op, e) ->
      let t = infer_type env_ref e in
      (match op with
       | NegateUnOp -> unify env_ref t IntTy; IntTy
       | NotUnOp -> unify env_ref t BoolTy; BoolTy)
  | BinOp (op, e1, e2) ->
      let t1 = infer_type env_ref e1 in
      let t2 = infer_type env_ref e2 in
      (match op with
       | Add | Sub | Mul | Div | Modulo -> unify env_ref t1 IntTy; unify env_ref t2 IntTy; IntTy
       | AndBinOp | OrBinop -> unify env_ref t1 BoolTy; unify env_ref t2 BoolTy; BoolTy
       | Equal -> unify env_ref t1 t2; BoolTy
       | LessThan -> unify env_ref t1 IntTy; unify env_ref t2 IntTy; BoolTy
       | Concatenation -> unify env_ref t1 StringTy; unify env_ref t2 StringTy; StringTy)

  | IfThenElse (e1, e2, e3) ->
      let cond_type = infer_type env_ref e1 in
      unify env_ref cond_type BoolTy;
      let then_type = infer_type env_ref e2 in
      let else_type = infer_type env_ref e3 in
      unify env_ref then_type else_type;
      then_type

  | TupleExpr (e, es) ->
      let t = infer_type env_ref e in
      let ts = List.map (infer_type env_ref) es in
      TupleTy (t :: ts)

  | LetFun (id, params_opt, ret_type_opt, body, in_expr) ->
      let update_param_types_option (env : type_env) (params: param list option) : type_env =
        match params with
        | None -> env
        | Some params -> 
        let rec helper (env : type_env) (params: param list) : type_env =
            match params with
            | [] -> env
            | SimpleParam id :: params -> 
                let var = fresh_var () in
                let env = StringMap.add id (VarTy var) env in
                helper env params
            | TypedParam (id, t) :: params ->
                let env = StringMap.add id (convert_type t) env in
                helper env params in
        helper env params in
      let new_env = update_param_types_option !env_ref params_opt in
      let inferred_ret_type = match ret_type_opt with
        | Some t -> convert_type t
        | None -> VarTy (fresh_var ()) in

      let new_env = StringMap.add id inferred_ret_type new_env in
      let env_ref = ref new_env in
      let inferred_body_type = infer_type env_ref body in
      unify env_ref inferred_ret_type inferred_body_type;

      let fun_type = match params_opt with
        | None -> StringMap.find id !env_ref
        | Some params -> 
            let param_types = List.map (fun p -> match p with
              | SimpleParam type_id -> StringMap.find type_id !env_ref
              | TypedParam (type_id, _) -> StringMap.find type_id !env_ref) params in
            List.fold_right (fun param_type acc -> FuncTy (param_type, acc)) param_types (StringMap.find id !env_ref) in
      
      let env_with_fun = StringMap.add id fun_type !env_ref in     
      let generalized_fun_type = generalize env_with_fun fun_type in
      let env_with_generalized_fun = StringMap.add id generalized_fun_type env_with_fun in
      infer_type (ref env_with_generalized_fun) in_expr
  
  | LetRec (id, params_opt, ret_type_opt, body, in_expr) ->
      let update_param_types_option (env : type_env) (params: param list option) : type_env =
        match params with
        | None -> env
        | Some params -> 
        let rec helper (env : type_env) (params: param list) : type_env =
            match params with
            | [] -> env
            | SimpleParam id :: params -> 
                let var = fresh_var () in
                let env = StringMap.add id (VarTy var) env in
                helper env params
            | TypedParam (id, t) :: params ->
                let env = StringMap.add id (convert_type t) env in
                helper env params in
        helper env params in
      let new_env = update_param_types_option !env_ref params_opt in
      let inferred_ret_type = match ret_type_opt with
        | Some t -> convert_type t
        | None -> VarTy (fresh_var ()) in

      let fun_type = match params_opt with
        | None -> inferred_ret_type
        | Some params -> 
            let param_types = List.map (fun p -> match p with
              | SimpleParam type_id -> StringMap.find type_id new_env
              | TypedParam (type_id, _) -> StringMap.find type_id new_env) params in
            List.fold_right (fun param_type acc -> FuncTy (param_type, acc)) param_types inferred_ret_type in

      let env_with_fun = StringMap.add id fun_type new_env in
      env_ref := env_with_fun;

      let inferred_body_type = infer_type env_ref body in
      unify env_ref inferred_ret_type inferred_body_type;
      
      let fun_type = StringMap.find id !env_ref in
      let generalized_fun_type = generalize !env_ref fun_type in
      let env_with_generalized_fun = StringMap.add id generalized_fun_type !env_ref in
      infer_type (ref env_with_generalized_fun) in_expr
  
  | FunCall (func_expr, arg_expr) ->
      let func_type = infer_type env_ref func_expr in
      let arg_type = infer_type env_ref arg_expr in
      (match instantiate func_type with
       | FuncTy (param_type, return_type) ->
          unify env_ref param_type arg_type;
          return_type
       | _ -> raise (TypeError "Attempted to call a non-function value"))

  | Function (params, ret_type_opt, body) ->
       (* Create a new environment for the function scope *)
       let func_env = env_ref in
   
       (* Convert parameters to type expressions and add them to the environment *)
       List.iter (function
       | SimpleParam id -> 
           let var_type = VarTy (fresh_var ()) in
           func_env := StringMap.add id var_type !func_env
       | TypedParam (id, t) -> 
           let t_type = convert_type t in
           func_env := StringMap.add id t_type !func_env
       ) params;
   
       (* Infer the return type of the function body *)
       let inferred_body_type = infer_type func_env body in
   
       (* Determine the return type of the function *)
       let return_type = match ret_type_opt with
           | Some ret_type -> convert_type ret_type
           | None -> inferred_body_type in
   
       (* Ensure the inferred body type matches the declared return type, if any *)
       if ret_type_opt <> None then
           unify func_env inferred_body_type return_type;
 
       (* Construct the function type *)
       let fun_type = List.fold_right (fun p acc ->
       match p with
       | SimpleParam id | TypedParam (id, _) ->
           let param_type = StringMap.find id !func_env in
               FuncTy (param_type, acc)
       ) params return_type in
       fun_type

  (* For each branch, construct as a function call *)
  | MatchWith (match_expr, branches) ->
    let match_type = infer_type env_ref match_expr in
    match branches with
    | [] -> raise (TypeError "Match expression must have at least one branch")
    | _ -> 
        let check_branch env_ref (Branch (constructor, var, expr)) =
          let constructor_type = StringMap.find constructor !env_ref in
          match var with
          | None -> 
              unify env_ref match_type constructor_type;
              let return_type = infer_type env_ref expr in
              return_type
          | Some var -> match var with
              | SingleVar id ->       
                let constructor_type = StringMap.find constructor !env_ref in
                let temp_env = StringMap.add id (VarTy (fresh_var ())) !env_ref in
                let env_ref = ref temp_env in
                let var_type = let func_type = constructor_type in
                  let arg_type = infer_type env_ref (Identifier id) in
                  (match instantiate func_type with
                  | FuncTy (param_type, return_type) ->
                      unify env_ref param_type arg_type;
                      return_type
                  | _ -> raise (TypeError "Attempted to call a non-function value")) in
                unify env_ref match_type var_type;
                let return_type = infer_type env_ref expr in
                return_type
              | TupleVar (var, vars) ->
                let constructor_type = StringMap.find constructor !env_ref in
                let temp_env = StringMap.add var (VarTy (fresh_var ())) !env_ref in
                let temp_env = List.fold_left (fun env var -> StringMap.add var (VarTy (fresh_var ())) env) temp_env vars in
                let env_ref = ref temp_env in
                let var_type = let func_type = constructor_type in
                let arg_type = infer_type env_ref (TupleExpr (Identifier var, List.map (fun v -> Identifier v) vars)) in
                  (match instantiate func_type with
                  | FuncTy (param_type, return_type) ->
                      print_endline "BUG!";
                      unify env_ref param_type arg_type;
                      return_type
                  | _ -> raise (TypeError "Attempted to call a non-function value")) in
                unify env_ref match_type var_type;
                let return_type = infer_type env_ref expr in
                return_type
        in
      let branch_types = List.map (check_branch env_ref) branches in
      match branch_types with
      | first_branch_type :: other_branch_types ->
          List.iter (fun branch_type -> unify env_ref first_branch_type branch_type) other_branch_types;
          first_branch_type
      | [] -> failwith "Unreachable, since branches cannot be empty"

(* A : int -> T *)
let type_declaration (env: type_env) (type_def: type_decl) (decl_name : identifier): type_env =
    match type_def with
    | TypeDecl (id, t_opt) ->
        match t_opt with
          | None -> StringMap.add (id) (UserTy decl_name) env
          | Some t -> StringMap.add (id) (FuncTy((convert_type t), UserTy decl_name)) env

(* Main entry point *)
let typecheck_binding (b : binding) (env : type_env) : type_env = 
    match b with
    | LetBinding (id, params_opt, ret_type_opt, expr) ->
      let original_env = env in
      let env_ref = ref env in  
      let update_param_types_option (env : type_env) (params: param list option) : type_env =
        match params with
        | None -> env
        | Some params -> 
        let rec helper (env : type_env) (params: param list) : type_env =
            match params with
            | [] -> env
            | SimpleParam id :: params -> 
                let var = fresh_var () in
                let env = StringMap.add id (VarTy var) env in
                helper env params
            | TypedParam (id, t) :: params ->
                let env = StringMap.add id (convert_type t) env in
                helper env params in
        helper env params in
      let new_env = update_param_types_option !env_ref params_opt in        
      let inferred_ret_type = match ret_type_opt with
        | Some t -> convert_type t
        | None -> VarTy (fresh_var ()) in

      let new_env = StringMap.add id inferred_ret_type new_env in
      let env_ref = ref new_env in
      let _ = infer_type env_ref expr in
      let inferred_body_type = infer_type env_ref expr in
      unify env_ref inferred_ret_type inferred_body_type;

      let fun_type = match params_opt with
        | None -> StringMap.find id !env_ref
        | Some params -> 
            let param_types = List.map (fun p -> match p with
              | SimpleParam type_id -> StringMap.find type_id !env_ref
              | TypedParam (type_id, _) -> StringMap.find type_id !env_ref) params in
            List.fold_right (fun param_type acc -> FuncTy (param_type, acc)) param_types (StringMap.find id !env_ref) in

      print_endline "Function type: ";
      print_type fun_type;
      let env_with_fun = StringMap.add id fun_type original_env in     
      let generalized_fun_type = generalize env_with_fun fun_type in
      let env_with_generalized_fun = StringMap.add id generalized_fun_type env_with_fun in
      env_with_generalized_fun

    | LetRecBinding (id, params_opt, ret_type_opt, expr) ->  
      let original_env = env in
      let env_ref = ref env in
    
        let update_param_types_option (env : type_env) (params: param list option) : type_env =
          match params with
          | None -> env
          | Some params -> 
          let rec helper (env : type_env) (params: param list) : type_env =
              match params with
              | [] -> env
              | SimpleParam id :: params -> 
                  let var = fresh_var () in
                  let env = StringMap.add id (VarTy var) env in
                  helper env params
              | TypedParam (id, t) :: params ->
                  let env = StringMap.add id (convert_type t) env in
                  helper env params in
          helper env params in
        let new_env = update_param_types_option !env_ref params_opt in
        let inferred_ret_type = match ret_type_opt with
          | Some t -> convert_type t
          | None -> VarTy (fresh_var ()) in

        let fun_type = match params_opt with
          | None -> FuncTy(UnitTy, inferred_ret_type)
          | Some params -> 
              let param_types = List.map (fun p -> match p with
                | SimpleParam type_id -> StringMap.find type_id new_env
                | TypedParam (type_id, _) -> StringMap.find type_id new_env) params in
              List.fold_right (fun param_type acc -> FuncTy (param_type, acc)) param_types inferred_ret_type in

        let env_with_fun = StringMap.add id fun_type new_env in
        env_ref := env_with_fun;

        let _ = infer_type env_ref expr in
        let inferred_body_type = infer_type env_ref expr in
        unify env_ref inferred_ret_type inferred_body_type;
        
        let fun_type = StringMap.find id !env_ref in
        let generalized_fun_type = generalize !env_ref fun_type in
        let env_with_generalized_fun = StringMap.add id generalized_fun_type original_env in
        env_with_generalized_fun

    | TypeBinding (id, type_decls) ->
        let env = List.fold_left (fun env type_decl -> type_declaration env type_decl id) env type_decls in
        env

(* Main entry point *)
let typecheck_program (p : program): unit = 
    let rec helper (env : type_env) (p : program) : unit =
        print_endline "Environment: ";
        print_env env;
        match p with
        | [] -> ()
        | binding :: bindings -> let new_env = typecheck_binding binding env in
                                 helper new_env bindings 
    in
    helper initial_env p
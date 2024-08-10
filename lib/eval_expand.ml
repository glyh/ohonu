open Ast
open Scopes

exception Unimplemented
exception TypeError
exception NoSuchSym of string

let get_sub_env (s: scope_set) (e: environment) = 
  let s_new = ScopeSet.add (new_scope ()) s in
  ({
    outer = Some e;
    current = SymTbl.create 10
  }, s_new)

let rec lookup_env (sym: string) (s: scope_set) (e: environment): value option = 
  match (e.outer, resolve_scope_map e.current sym s) with
  | (_, Some v) -> Some v
  | (Some outer, None) -> lookup_env sym s outer
  | (_, _) -> None

let rec eval_terms (s: scope_set) (env: environment) (terms: form list): value list = 
  List.map (eval s env) terms

and eval (s: scope_set) (env: environment) (term: form): value = 
  match term with
  | Let(sym, rhs, inner) -> 
      let (sub_env, sub_scope_set) = get_sub_env s env in
      let rhs_evaled = (eval s env rhs) in
      push_scope rhs_evaled sym sub_scope_set sub_env.current;
      eval sub_scope_set sub_env inner

  | LetSyntax(sym, rhs, inner) ->
      let (sub_env, sub_scope_set) = get_sub_env s env in
      let st_evaled = begin match eval s env rhs with
      | Lambda fn -> 
          let wrapped = fun s_call_site vs ->
            let scopes_new = ScopeSet.union s s_call_site in
            match fn scopes_new env vs with
            | Quote f -> f
            | _ -> raise TypeError
          in (ST wrapped)
      | _ -> raise TypeError
      end
      in 
      push_scope st_evaled sym sub_scope_set sub_env.current;
      eval sub_scope_set sub_env inner

  | List(Val (Sym sym), rest) ->
      begin match lookup_env sym s env with
      | Some(ST(f_st)) -> 
          let expanded = f_st s (List.map (fun f -> Quote f) rest)
          in eval s env expanded
      | Some(Lambda(fn)) -> 
          fn s env (eval_terms s env rest)
      | Some(_) -> raise TypeError
      | None -> raise (NoSuchSym sym)
      end

  | List(l, rest) -> 
      begin match eval s env l with
      | Lambda(fn) ->
        let rest_evaled = eval_terms s env rest in
        fn s env rest_evaled
      | _ -> raise TypeError
      end
  | Val(Sym sym) -> 
      begin match lookup_env sym s env with
      | Some(v) -> 
          v
      | None -> raise (NoSuchSym sym)
      end

  | Val(v) -> v

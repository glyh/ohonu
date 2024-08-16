open Ast
open Scopes

exception Unimplemented
exception TypeError
exception SyntaxError
exception NoSuchSym of string

let get_sub_env (s: scope_set) (e: 'a environment_t) = 
  ScopeSet.add (new_scope ()) s,
  {
    outer = Some e;
    current = SymTbl.create 10
  }

type expand_env = ast_expanded environment_t

let rec lookup_env (sym: string) (s: scope_set) (e: 'a environment_t): 'a option = 
  match (e.outer, resolve_scope_map e.current sym s) with
  | (_, Some v) -> Some v
  | (Some outer, None) -> lookup_env sym s outer
  | (_, _) -> None

  (* NOTE: 
      Syntax eval is needed because the semantic is a bit different in various
      ways:
          1. for let binding, the bounding position maybe resolved to a syntax
          object passed in.
   *)
let rec syntax_eval (scopes: scope_set) (env: expand_env) (term: value): ast_expanded = 
    match term with
    | List [Sym "let"; Sym sym; rhs; inner] -> 
        let (scopes', env') = get_sub_env scopes env in
        let rhs_evaled = (syntax_eval scopes env rhs) in
        let sym_resolved = begin match lookup_env sym scopes env with
        | Some (ESym (sym', sym_scope')) -> ESym (sym', ScopeSet.union sym_scope' scopes)
        | None -> sym, scopes
        end
        in
        push_scope rhs_evaled sym_resolved scopes' env'.current;
        syntax_eval scopes' env' inner
    | _ -> raise Unimplemented
(* NOTE:
   1. We need to pass on the wrapping environment, because we have to be able to
   resolve values referenced from inside a syntax transformer
*)
(*and expand_macro (scopes: scope_set) (env: environment) (term: value): ast_expanded =*)
(*  match term with*)
(*  | List [Sym "let"; Sym sym; rhs; inner] ->*)
(*      let (scopes', env') = get_sub_env scopes env in*)
(*      let rhs_evaled = (syntax_eval scopes env rhs) in*)
(*      push_scope rhs_evaled sym scopes' env'.current;*)
(*      ELet((sym, scopes'), expand_macro scopes' env' rhs, expand_macro scopes' env' inner)*)
(*  | List [Sym "let-syntax"; Sym sym; rhs; inner] ->*)
(*      let (scopes', env') = get_sub_env scopes env in*)
(*      let st = begin match expand_macro scopes env rhs with*)
(*          | EST fn -> ST fn*)
(*          | _ -> raise TypeError*)
(*          end*)
(*      in*)
(*      push_scope st sym scopes' env'.current;*)
(*      expand_macro scopes' env' inner*)
(*  | List [Sym "syntax"; List bindings; body] ->*)
(*      let wrapped scopes environment forms  = *)
(*          let (scopes', env') = get_sub_env scopes env in*)
(*          List.combine bindings forms*)
(*          |> List.map (fun (arg, param) -> push_scope param arg scopes' env'.current)*)
(*          syntax_eval scopes' env' form*)

(*and eval_terms (s: scope_set) (env: environment) (terms: value list): value list = *)
(*  List.map (eval s env) terms*)
(**)
(*and eval (s: scope_set) (env: environment) (term: value): value = *)
(*  match term with*)
(*  | List [Sym "let"; Sym sym; rhs; inner ] ->*)
(*      let (sub_env, sub_scope_set) = get_sub_env s env in*)
(*      let rhs_evaled = (eval s env rhs) in*)
(*      push_scope rhs_evaled sym sub_scope_set sub_env.current;*)
(*      eval sub_scope_set sub_env inner*)
(**)
(*  | List [Sym "let-syntax"; Sym sym; rhs; inner ] ->*)
(*      let (sub_env, sub_scope_set) = get_sub_env s env in*)
(*      let st_evaled = begin match eval s env rhs with*)
(*      | Lambda fn -> *)
(*          let wrapped = fun s_call_site vs ->*)
(*            let scopes_new = ScopeSet.union s s_call_site in*)
(*              fn scopes_new env vs*)
(*          in (ST wrapped)*)
(*      | _ -> raise TypeError*)
(*      end*)
(*      in *)
(*      push_scope st_evaled sym sub_scope_set sub_env.current;*)
(*      eval sub_scope_set sub_env inner*)
(**)
(*  | List[Sym "fn"; List params; body] ->*)
(*    let collect_params v =*)
(*        begin match v with*)
(*        | Sym s -> s*)
(*        | _ -> raise TypeError*)
(*        end*)
(*    in*)
(*    let collected_params = List.map collect_params params in*)
(*    let wrapped s env args =*)
(*      let (sub_env, sub_scope_set) = get_sub_env s env in*)
(*      let binded_args = (List.combine collected_params args) in*)
(*      List.map (fun (sym, arg) -> push_scope arg sym sub_scope_set sub_env.current) binded_args |> ignore;*)
(*      eval sub_scope_set sub_env body*)
(*    in Lambda wrapped*)
(**)
(*  | List(Sym ("let" | "let-syntax" | "fn") :: _) ->*)
(*          raise SyntaxError*)
(**)
(*  | List(Sym sym :: rest) ->*)
(*      begin match lookup_env sym s env with*)
(*      | Some(ST(f_st)) -> *)
(*          let expanded = f_st s rest*)
(*          in eval s env expanded*)
(*      | Some(Lambda(fn)) -> *)
(*          fn s env (eval_terms s env rest)*)
(*      | Some(_) -> raise TypeError*)
(*      | None -> raise (NoSuchSym sym)*)
(*      end*)
(**)
(*  | List(l :: rest) -> *)
(*      begin match eval s env l with*)
(*      | Lambda(fn) ->*)
(*        let rest_evaled = eval_terms s env rest in*)
(*        fn s env rest_evaled*)
(*      | _ -> raise TypeError*)
(*      end*)
(*  | Sym sym -> *)
(*      begin match lookup_env sym s env with*)
(*      | Some(v) -> *)
(*          v*)
(*      | None -> raise (NoSuchSym sym)*)
(*      end*)
(**)
(*  | v -> v*)

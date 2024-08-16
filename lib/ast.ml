open Scopes

type sym = string * ScopeSet.t

type 'a environment_t = {
  current: 'a scope_map;
  outer: 'a environment_t option
}

type cst = 
  | CString of string
  | CSym of string
  | CBool of bool
  | CInt of int
  | CList of cst list

type ast_sym = string * scope_set
type environment = value environment_t

and value = 
  | String of string
  | Sym of ast_sym
  | Bool of bool
  | Int of int
  | Lambda of (scope_set -> environment -> value list -> value)
  (*
    The environment here is for macro expansion time evaluation, rather than the runtime evlauation environment
  *)
  | ST of (scope_set -> environment -> value list -> value)
  | List of value list
  | Nil
and ast = 
  | ALet of ast_sym * ast * ast
  | ALs of ast_sym * ast * ast
  | ALambda of ast_sym list * ast
  | AVal of value
  | AApp of ast list

let quote_char = function
  | '\n' -> "\\n"
  | '\\' -> "\\\\"
  | c -> String.init 1 (fun _ -> c)

let quote_string (s: string): string =
  s |> String.to_seq |> Seq.map quote_char |> List.of_seq |> String.concat ""

let rec string_of_value = function
  | List lst -> "(" ^ (List.map string_of_value lst |> String.concat " ") ^ ")"
  | String s -> "\"" ^ (quote_string s) ^ "\""
  | Sym (sym, _) -> sym
  | Bool true -> "true"
  | Bool false -> "false"
  | Int i -> string_of_int i
  | Lambda _ -> "[lambda]"
  | ST _ -> "[st]"
  | Nil -> "nil"

exception ExpectingSym 

let rec cst_to_ast (t: cst): ast = 
  match t with
  | CString(s) -> AVal(String(s))
  | CSym(s) -> AVal(Sym(s, ScopeSet.empty))
  | CBool(b) -> AVal(Bool(b))
  | CInt(i) -> AVal(Int(i))
  | CList [CSym("let"); CSym(id); rhs; body] -> ALet((id, ScopeSet.empty), cst_to_ast rhs, cst_to_ast body)
  | CList [CSym("ls"); CSym(id); rhs; body] -> ALs((id, ScopeSet.empty), cst_to_ast rhs, cst_to_ast body)
  | CList [CSym("lambda"); CList syms; body] -> 
      let get_param (sym: cst) = 
        begin match sym with
        | CSym s -> (s, ScopeSet.empty)
        | _ -> raise ExpectingSym
        end
      in let get_params = List.map get_param
      in 
      ALambda(get_params syms, cst_to_ast body)
  | CList items -> AApp(List.map cst_to_ast items)

let flip_scope (s: scope) (ss: scope_set) =
  match ScopeSet.find_opt s ss with
  | Some _ -> ScopeSet.remove s ss
  | None -> ScopeSet.add s ss

let add_scope = ScopeSet.add
let remove_scope = ScopeSet.remove

let rec apply_to_scope (f: scope_set -> scope_set) (a: ast): ast =
  let f1 (sym, scopes) = (sym, f scopes) in
  let frec = apply_to_scope f in
  match a with
  | ALet(asym, rhs, inner) -> ALet(f1 asym, frec rhs, frec inner)
  | ALs(asym, rhs, inner) -> ALs(f1 asym, frec rhs, frec inner)
  | ALambda(syms, body) ->
      let f1s = List.map f1 in
      ALambda(f1s syms, frec body)
  | AVal(Sym asym) -> AVal(Sym (f1 asym))
  | AVal _ -> a
  | AApp (lst) -> AApp(List.map frec lst)

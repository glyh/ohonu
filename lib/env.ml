open Ast
open Scopes

exception ArityError of string * int * int (* expected, actual*)
exception ValueTypeMismatch of string * string * string

let wrap_lam (fn: value list -> value) =
  Lambda(fun _ _ vs -> fn vs)

let v_numeric id op vs =
  match vs with
  | [Int a; Int b] -> Int (op a b)
  | [_ as operand; Int _] | [_; _ as operand] -> 
      raise (ValueTypeMismatch (id, get_type operand, "int"))
  | _ -> raise (ArityError (id, (List.length vs), 2))

let base_env: environment = {
  outer = None;
  current = 
    [
      "+", [ScopeSet.empty, wrap_lam (v_numeric "+" (+))];
      "-", [ScopeSet.empty, wrap_lam (v_numeric "-" (-))];
      "*", [ScopeSet.empty, wrap_lam (v_numeric "*" ( * ))];
      "/", [ScopeSet.empty, wrap_lam (v_numeric "/" (/))];
    ] |> List.to_seq |> SymTbl.of_seq
}


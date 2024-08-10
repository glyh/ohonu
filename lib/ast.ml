open Scopes

type sym = string * ScopeSet.t

type environment = {
  current: value scope_map;
  outer: environment option
}

and value = 
  | String of string
  | Sym of string
  | Bool of bool
  | Int of int
  | Lambda of (scope_set -> environment -> value list -> value)
  | ST of (scope_set -> value list -> value)
  | List of value list

let quote_char = function
  | '\n' -> "\\n"
  | '\\' -> "\\\\"
  | c -> String.init 1 (fun _ -> c)

let quote_string (s: string): string =
  s |> String.to_seq |> Seq.map quote_char |> List.of_seq |> String.concat ""

let rec string_of_value = function
  | List lst -> "(" ^ (List.map string_of_value lst |> String.concat " ") ^ ")"
  | String s -> "\"" ^ (quote_string s) ^ "\""
  | Sym sym -> sym
  | Bool true -> "true"
  | Bool false -> "false"
  | Int i -> string_of_int i
  | Lambda _ -> "[lambda]"
  | ST _ -> "[st]"

let get_type (v: value): string = 
  match v with
  | List _ -> "list"
  | String _ -> "string"
  | Sym _ -> "sym"
  | Bool _ -> "bool"
  | Int _ -> "int"
  | Lambda _ -> "lambda"
  | ST _ -> "st"

open Scopes

type sym = string * ScopeSet.t

type environment = {
  current: value scope_map;
  outer: environment option
}

and value = 
  | Quote of form
  | String of string
  | Sym of string
  | Bool of bool
  | Int of int
  | Lambda of (scope_set -> environment -> value list -> value)
  | ST of (scope_set -> value list -> form)

and form = 
  | List of form * form list
  | Val of value
  | Let of string * form * form
  | LetSyntax of string * form * form

let quote_char = function
  | '\n' -> "\\n"
  | '\\' -> "\\\\"
  | c -> String.init 1 (fun _ -> c)

let quote_string (s: string): string =
  s |> String.to_seq |> Seq.map quote_char |> List.of_seq |> String.concat ""

let rec string_of_form = function
  | List (hd, rest) -> "(" ^ (List.map string_of_form (hd :: rest) |> String.concat " ") ^ ")"
  | Val (v) -> string_of_value v
  | Let(id, rhs, body) -> "(let " ^ id ^ (string_of_form rhs) ^ (string_of_form body) ^ ")"
  | LetSyntax(id, rhs, body) -> "(let-syntax " ^ id ^ (string_of_form rhs) ^ (string_of_form body) ^ ")"
and string_of_value = function
  | Quote f -> "'" ^ string_of_form f
  | String s -> "\"" ^ (quote_string s) ^ "\""
  | Sym sym -> sym
  | Bool true -> "true"
  | Bool false -> "false"
  | Int i -> string_of_int i
  | Lambda _ -> "[lambda]"
  | ST _ -> "[st]"

let get_type (v: value): string = 
  match v with
  | Quote _ -> "form"
  | String _ -> "string"
  | Sym _ -> "sym"
  | Bool _ -> "bool"
  | Int _ -> "int"
  | Lambda _ -> "lambda"
  | ST _ -> "st"

open Scopes
open Eval_expand
open Ast

let read source = 
  let parsed = Parser.parse_string source in
  parsed

let print (value: value) =
  value |> string_of_value |> print_endline

let rep input = 
  try 
    (input 
    |> read 
    |> eval ScopeSet.empty Env.base_env 
    |> print
    ) with
  | NoSuchSym(sym) -> print_endline (Printf.sprintf "Can't find symbol `%s`." sym)

let interactive () =
  while true do
    read_line () |> rep
  done


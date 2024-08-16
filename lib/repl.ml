open Scopes
open Eval_expand
open Ast

let read source = 
  source
  |> Parser.parse_string
  |> cst_to_ast

let print (value: value) =
  value |> string_of_value |> print_endline

let read_and_eval source =
  source |> read |> eval ScopeSet.empty Env.base_env 


let rep input = 
  try 
    (input 
    |> read_and_eval
    |> print
    ) with
  | NoSuchSym(sym) -> print_endline (Printf.sprintf "Can't find symbol `%s`." sym)

let interactive () =
  while true do
    read_line () |> rep
  done

let%test "simple" =
 0 == compare (Int 1) (read_and_eval "(let x 1 (let-syntax m (fn () x) (let x 2 (m))))")



(*

  Finding smallest superset takes O(N max(Q_i)), where N is the number of
  bindings for a specific symbol available, and Q_i is the number of the scope
  related to that specific symbol.

  Could achieve O(P), where P is the size of querying symbol, by constructing a
  DASG

 *)

type scope = int

let new_scope: unit -> scope =
  let cnt = ref 0 in
  fun () ->
    cnt := !cnt + 1;
    !cnt

module ScopeSet = Set.Make(struct 
  type t = scope
  let compare = compare
end)

module SymMap = Map.Make(String)

module SymTbl = Hashtbl.Make(struct
  type t = string
  let equal = String.equal
  let hash = Hashtbl.hash
end)

type scope_set = ScopeSet.t
type 'ty scope_sets = (scope_set * 'ty) list (* This should be replaced with a DASG to speed it up. *)
type 'ty scope_map = 'ty scope_sets SymTbl.t

exception AmbiguousScopeRsolution of scope_set * scope_set * scope_set

(* The index is marked in revese order, so when inserting a new scope_set we put it at front *)
let resolve_scope_set (ss: 'ty scope_sets) (s: ScopeSet.t): 'ty option =
  let rec resolve_scope_set_inner (ss: 'ty scope_sets) (s: ScopeSet.t): ('ty * scope_set) option =
    match ss with
    | [] -> None
    | (si, v) :: s_rest ->
        if ScopeSet.subset si s then
          begin match resolve_scope_set_inner s_rest s with
          | None -> (Some (v, si))
          | Some(id, sj) -> 
              let cmp = compare (ScopeSet.cardinal si) (ScopeSet.cardinal sj) in
              if cmp < 0 then Some(v, si)
              else if cmp > 0 then Some(id, sj)
              else raise (AmbiguousScopeRsolution(s, si, sj))
          end
        else 
          resolve_scope_set_inner s_rest s
  in
  match resolve_scope_set_inner ss s with
  | None -> None
  | Some(v, _) -> Some v

let resolve_scope_map (m: 'ty scope_map) (symbol_name: string) (s: ScopeSet.t): 'ty option =
  match SymTbl.find_opt m symbol_name with
  | None -> None
  | Some(ss) -> resolve_scope_set ss s

let push_scope (v: 'a) (name: string) (scope: scope_set) (m: 'a scope_map) =
  match SymTbl.find_opt m name with
  | None -> SymTbl.add m name [scope, v]
  | Some(scope_sets) -> 
      SymTbl.add m name ((scope, v) :: scope_sets )

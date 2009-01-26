
open Ast

exception Compile_error

type typ = IntType

module Context = Map.Make(String)

let rec build_context cntxt decls =
  match decls with
    (name, t) :: rest ->
      build_context (Context.add name IntType cntxt) rest
    | [] -> cntxt

let compile cntxt stmt =
  match stmt with
    ExprStatement (Declaration decls) -> ((build_context cntxt decls), [])
  | _ -> (cntxt, [])

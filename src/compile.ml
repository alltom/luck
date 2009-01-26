
exception Compile_error

type typ =
  ArrayType of typ
| RefType of typ
| IntType

module Context = Map.Make(String)

exception Type_conversion

let rec string_of_type t =
  match t with
    ArrayType t' -> (string_of_type t') ^ "[]"
  | RefType t' -> (string_of_type t') ^ "@"
  | IntType -> "int"

let rec convert_type asttype =
  match asttype with
    Ast.Type("int", false, _, []) -> IntType
  | Ast.Type(b, false, s, h::r) ->
      ArrayType(convert_type (Ast.Type(b, false, s, r)))
  | _ -> raise Type_conversion

let rec build_context cntxt decls =
  match decls with
    (name, t) :: rest ->
      build_context (Context.add name (convert_type t) cntxt) rest
    | [] -> cntxt

let compile cntxt stmt =
  match stmt with
    Ast.ExprStatement (Ast.Declaration decls) -> ((build_context cntxt decls), [])
  | _ -> (cntxt, [])

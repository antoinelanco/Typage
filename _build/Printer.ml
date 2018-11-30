open Type
open Printf

let rec printtyp (t:SimpleTypes.typ) : string =
  match t with
  | TVar s -> s
  | TInt -> "int"
  | TBool -> "bool"
  | TUnit -> "()"
  | TFun (t1,t2) -> sprintf "%s -> %s" (printtyp t1) (printtyp t2)
  | TPair (t1,t2) -> sprintf "%s * %s" (printtyp t1) (printtyp t2)
  | TRef  t -> sprintf "%s ref" (printtyp t)


let rec printtypO (t:OptionTypes.typ) : string =
  match t with
  | TVar s -> s
  | TInt -> "int"
  | TBool -> "bool"
  | TUnit -> "()"
  | TFun (t1,t2) -> sprintf "%s -> %s" (printtypO t1) (printtypO t2)
  | TPair (t1,t2) -> sprintf "%s * %s" (printtypO t1) (printtypO t2)
  | TRef  t -> sprintf "%s ref" (printtypO t)
  | TMaybe t -> sprintf "%s option" (printtypO t)

module Env = Map.Make(String)
type type_env = SimpleTypes.typ Env.t

let printenv = Env.iter (fun k el -> printf "%s : %s\n" (printtyp el) k)

let printOption = function
  | Some a -> printtypO a
  | None -> "None"

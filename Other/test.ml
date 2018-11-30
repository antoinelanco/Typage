open Printf

module SimpleTypes = struct

  type typ =
    | TInt
    | TBool
    | TUnit
    | TVar  of string
    | TFun  of typ * typ
    | TPair of typ * typ
    | TRef  of typ

end

module RawAST = struct

  type expression =
    | Unit
    | Int    of int
    | Bool   of bool
    | Var    of string
    | App    of expression * expression
    | Fun    of string * expression
    | Let    of string * expression * expression
    | Op     of string
    | Pair   of expression * expression
    | Fst    of expression
    | Snd    of expression
    | NewRef of expression
    | DeRef  of expression
    | Sequence of expression * expression
    | If     of expression * expression * expression
    | While  of expression * expression

end

module Env = Map.Make(String)
type type_env = SimpleTypes.typ Env.t

let rec print (t:SimpleTypes.typ) : string =
  match t with
  | TVar s -> s
  | TInt -> "int"
  | TBool -> "bool"
  | TUnit -> "()"
  | TFun (t1,t2) -> sprintf "%s -> %s" (print t1) (print t2)
  | TPair (t1,t2) -> sprintf "%s * %s" (print t1) (print t2)
  | TRef  t -> sprintf "%s ref" (print t)


let printenv = Env.iter (fun k el -> printf "%s : %s\n" (print el) k)




open SimpleTypes

let env = List.fold_left (fun acc (k,e) -> Env.add k e acc) Env.empty
    [("x",TUnit);("y",TInt);("a",TFun (TBool,TInt))]



let merge_vars k v1 v2 =
  match v1, v2 with
  | Some v1, None -> Some v1
  | None, Some v2 -> Some v2
  | None, None -> None
  | Some v1, Some v2 when v1 = v2 -> Some v1
  | _ -> failwith "2 type dif pour 1 var"

let rec f (t1:SimpleTypes.typ) (t2:SimpleTypes.typ) (env:type_env) : type_env =
  match t1,t2 with
  | TVar a ,t -> Env.map (fun el -> if el = TVar a then t else el ) env
  | t ,TVar a -> env
  | TFun (t1,t2), TFun(t3,t4) -> Env.merge merge_vars (f t1 t3 env) (f t2 t4 env)
  | TPair (t1,t2), TPair (t3,t4) ->
    Env.merge merge_vars (f t1 t3 env) (f t2 t4 env)
  | TRef t1, TRef t2 -> f t1 t2 env
  | r1,r2 when r1 = r2 -> env
  | _ -> failwith "Type Mismatch"


open RawAST



let a = TPair (TVar "'a", TVar "'a")
let b = TPair (TInt, TInt)


let res = f a b (Env.singleton "x" (TVar "'a"))
let () = printenv res

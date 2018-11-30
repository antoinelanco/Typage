module BaseTypeReconstruction = struct
  open SimpleTypes
  open RawAST

module Env = Map.Make(String)
type type_env = SimpleTypes.typ Env.t


let fresh_type_variable =
  let cpt = ref 64 in
  fun () -> incr cpt; Printf.sprintf "'%c" (Char.chr !cpt)



let rec comp (t1:typ) (t2:typ) : bool =
  match t1,t2 with
  | TVar _ ,_ -> true
  | _ ,TVar _ -> true
  | TFun (t1,t2), TFun(t3,t4) -> comp t1 t3 && comp t2 t4
  | TPair (t1,t2), TPair (t3,t4) -> comp t1 t3 && comp t2 t4
  | TRef t1, TRef t2 -> comp t1 t2
  | r1,r2 -> r1 = r2

let rec type_expression (env: type_env) (e: expression) : typ =
  match e with
  | Int _ -> TInt
  | Bool _ -> TBool
  | Unit -> TUnit
  | Var s -> Env.find s env




  | App (e1,e2) ->
    let ty1 = type_expression env e1 in
    let ty2 = type_expression env e2 in
    let ty_arg, ty_res = match ty1 with
      | TFun(ty_a, ty_r) -> ty_a, ty_r
      | _ -> failwith "TFun expected"
    in
    if comp ty_arg ty2
    then ty_res
    else failwith "Incompatible application"




  | Fun (s,e) ->
    let lab = fresh_type_variable () in
    let env' = Env.add s (TVar lab) env in
    let t_e = type_expression env' e in
    TFun(TVar lab,t_e)



  | Let (s,e1,e2)    ->
    let typ_e1 = type_expression env e1 in
    let env' = Env.add s typ_e1 env in
    type_expression env' e2
  | Pair (e1,e2) -> TPair(type_expression env e1,type_expression env e2)
  | Fst e ->
    begin
      match type_expression env e with
      | TPair (e1,e2) -> e1
      | _ -> failwith "Fst need TPair"
    end
  | Snd e ->
    begin
      match type_expression env e with
      | TPair (e1,e2) -> e2
      | _ -> failwith "Fst need TPair"
    end
  | NewRef e -> TRef (type_expression env e)
  | DeRef e ->
    begin
      match type_expression env e with
      | TRef r -> r
      | _ -> failwith "DeRef need TRef"
    end
  | Sequence (e1,e2) ->
    begin
      match type_expression env e1 with
      | TUnit -> type_expression env e2
      | _ -> failwith "First exp in Seq need Unit"
    end
  | If (e,e1,e2) ->
    let typ_e = type_expression env e in
    let typ_e1 = type_expression env e1 in
    let typ_e2 = type_expression env e2 in
    begin
      match typ_e,typ_e1,typ_e2 with
      | (TBool,t1,t2) when t1 = t2 -> t1
      | _ -> failwith "If Fail"
    end

  | While  (e1,e2) ->
    let typ_e1 = type_expression env e1 in
    let typ_e2 = type_expression env e2 in
    begin
      match typ_e1,typ_e2 with
      | TBool,TUnit -> TUnit
      | _ -> failwith"While Fail"
    end
  | Op s ->
    begin
      match s with
      | "+" | "-" | "*" | "/" -> TFun(TPair(TInt,TInt), TInt)
      | "<" | ">" | "==" |">=" | "<=" -> TFun(TPair(TInt,TInt), TBool)
      | "&&" | "||" -> TFun(TPair(TBool,TBool), TBool)
      | _ -> failwith "Op unknow"
    end
end

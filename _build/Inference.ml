open Type
open Printer
open Char
exception Etype of string



module BaseTypeChecker = struct
  open SimpleTypes
  open BaseAST
  (* EXO 1 *)

  module Env = Map.Make(String)
  type type_env = SimpleTypes.typ Env.t

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
      if ty_arg = ty2
      then ty_res
      else failwith "Incompatible application"
    | Fun (s,typ,e1) ->
      let env' = Env.add s typ env in
      let t_e = type_expression env' e1 in
      TFun(typ,t_e)
    | Let (s,e1,e2)    ->
      let typ_e1 = type_expression env e1 in
      let env' = Env.add s typ_e1 env in
      type_expression env' e2
    | Pair (e1,e2) -> TPair(type_expression env e1,type_expression env e2)
    | Fst e1 ->
      begin
        match type_expression env e1 with
        | TPair (e1,e2) -> e1
        | _ -> failwith "Fst need TPair"
      end
    | Snd e1 ->
      begin
        match type_expression env e1 with
        | TPair (e1,e2) -> e2
        | _ -> failwith "Fst need TPair"
      end
    | NewRef e1 -> TRef (type_expression env e1)
    | DeRef e1 ->
      begin
        match type_expression env e1 with
        | TRef r -> r
        | _ -> failwith "DeRef need TRef"
      end
    | Sequence (e1,e2) ->
      begin
        match type_expression env e1 with
        | TUnit -> type_expression env e2
        | _ -> failwith "First exp in Seq need Unit"
      end
    | If (ee,e1,e2) ->
      let typ_e = type_expression env ee in
      let typ_e1 = type_expression env e1 in
      let typ_e2 = type_expression env e2 in
      begin
        match typ_e,typ_e1,typ_e2 with
        | TBool,t1,t2 when t1 = t2 -> t1
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

module BaseTypeReconstruction = struct
  open SimpleTypes
  open RawAST


  (**
      Exercice 2 : inférence des types simples.

      Ci-dessous, une syntaxe quasi-identique à [BaseAST].
      A disparu : l'annotation du paramètre formel d'une fonction par son type.

      Objectif : inférence de types.
  *)

  (**
     Moteur d'inférence de types
  *)

  (**
      Objectif : compléter la fonction suivante de typage d'une expression.
      Un appel [type_expression e] doit :
      - renvoyer le type de [e] dans l'environnement [env] si [e] est bien typée
      - déclencher une exception sinon

      Procéder en deux étapes : génération de contraintes sur les types,
      puis résolution par unification.
  *)
  module Env = Map.Make(String)
  type type_env = SimpleTypes.typ Env.t


  let fresh_type_variable =
    let cpt = ref 64 in
    fun () -> if !cpt = 90 then cpt := 97 else incr cpt;
      Printf.sprintf "'%c" (Char.chr !cpt)



  let rec comp (t1:typ) (t2:typ) : bool =
    match t1,t2 with
    | TVar _ ,_ -> true
    | _ ,TVar _ -> true
    | TFun (t1,t2), TFun(t3,t4) -> comp t1 t3 && comp t2 t4
    | TPair (t1,t2), TPair (t3,t4) -> comp t1 t3 && comp t2 t4
    | TRef t1, TRef t2 -> comp t1 t2
    | r1,r2 -> r1 = r2


  let merge_vars k v1 v2 =
    match v1, v2 with
    | Some v1, None -> Some v1
    | None, Some v2 -> Some v2
    | None, None -> None
    | Some (TVar _), Some v2 -> Some v2
    | Some v1, Some (TVar _) -> Some v1
    | Some v1, Some v2 when v1 = v2 -> Some v1
    | _ -> failwith "2 type dif pour 1 var"

  let rec f (t1:SimpleTypes.typ) (t2:SimpleTypes.typ) (env:type_env) : type_env =
    (* let () = print_string (printtyp t1);print_string "\n";
       print_string (printtyp t2);print_string "\n" in *)
    match t1,t2 with
    | TVar a ,t -> Env.map (fun el -> if el = TVar a then t else el ) env
    | t ,TVar a -> env
    | TFun (t1,t2), TFun(t3,t4) -> Env.merge merge_vars (f t1 t3 env) (f t2 t4 env)
    | TPair (t1,t2), TPair (t3,t4) ->
      Env.merge merge_vars (f t1 t3 env) (f t2 t4 env)
    | TRef t1, TRef t2 -> f t1 t2 env
    | r1,r2 ->
      if r1 = r2
      then env
      else raise (Etype (Printf.sprintf "Type Mismatch %s != %s"
                           (printtyp r1)
                           (printtyp r2)))

  let rec type_expression (env: type_env) (e: expression) : typ * type_env =
    match e with
    | Int _ -> (TInt,env)
    | Bool _ -> (TBool,env)
    | Unit -> (TUnit,env)
    | Var s -> (Env.find s env,env)


    | App (e1,e2) ->
      let (ty1,env1) = type_expression env e1 in
      let (ty2,env2) = type_expression env1 e2 in
      let ty_arg, ty_res = match ty1 with
        | TFun(ty_a, ty_r) -> ty_a, ty_r
        | _ -> failwith "TFun expected"
      in
      let new_env = f ty2 ty_arg env2 in
      (ty_res,new_env)




    | Fun (s,e1) ->
      let lab = fresh_type_variable () in
      let env' = Env.add s (TVar lab) env in
      let (t_e,env'') = type_expression env' e1 in
      let t1 = Env.find s env'' in
      (TFun(t1,t_e),Env.remove s env'')



    | Let (s,e1,e2) ->
      let (typ_e1,env1) = type_expression env e1 in
      let env' = Env.add s typ_e1 env1 in
      type_expression env' e2

    | Pair (e1,e2) ->
      let (typ_e1,env1) = type_expression env e1 in
      let (typ_e2,env2) = type_expression env1 e2 in
      (TPair(typ_e1,typ_e2),env2)

    | Fst e1 ->
      let (typ_e1,env1) = type_expression env e1 in
      begin
        match typ_e1 with
        | TPair (e1,_) -> (e1,env1)
        | TVar a ->
          let lab1 = fresh_type_variable () in
          let lab2 = fresh_type_variable () in
          (TVar lab1, f (TVar a) (TPair(TVar lab1,TVar lab2)) env1)
        | _ -> failwith "Fst need TPair"
      end

    | Snd e1 ->
      let (typ_e1,env1) = type_expression env e1 in
      begin
        match typ_e1 with
        | TPair (e1,e2) -> (e2,env1)
        | TVar a ->
          let lab1 = fresh_type_variable () in
          let lab2 = fresh_type_variable () in
          (TVar lab2,f (TVar a) (TPair(TVar lab1,TVar lab2)) env1)
        | _ -> failwith "Fst need TPair"
      end

    | NewRef e1 -> let (e',env') = type_expression env e1 in (TRef e',env')

    | DeRef e1 ->
      let (typ_e,env') = type_expression env e1 in
      begin
        match typ_e with
        | TRef r -> (r,env')
        | TVar a ->
          let lab = fresh_type_variable () in
          (TVar lab, f (TVar a) (TRef (TVar lab)) env')
        | _ -> failwith "DeRef need TRef"
      end

    | Sequence (e1,e2) ->
      let (typ_e,env') = type_expression env e1 in
      begin
        match typ_e with
        | TUnit -> type_expression env' e2
        | TVar a ->
          let (typ_e2,env2) = type_expression env' e2 in
          (typ_e2, f (TVar a) TUnit env')
        | _ -> failwith "First exp in Seq need Unit"
      end

    | If (ee,e1,e2) ->
      let typ_e,env' = type_expression env ee in
      let typ_e1,env1 = type_expression env' e1 in
      let typ_e2,env2 = type_expression env1 e2 in
      begin
        match typ_e,typ_e1,typ_e2 with
        | TBool,t1,t2 when t1 = t2 -> (t1,env2)
        | TVar a,t1,t2 when t1 = t2 -> (t1, f (TVar a) TBool env2)
        | _ -> failwith "If Fail"
      end

    | While  (e1,e2) ->
      let (typ_e1,env1) = type_expression env e1 in
      let (typ_e2,env2) = type_expression env1 e2 in
      let r = match typ_e1,typ_e2 with
        | TBool,TUnit -> env2
        | TVar a,TUnit -> f (TVar a) TBool env2
        | TBool, TVar a -> f (TVar a) TUnit env2
        | TVar a, TVar b -> Env.merge merge_vars
                              (f (TVar a) TBool env2)
                              (f (TVar b) TUnit env2)
        | _ -> failwith "While Fail"
      in
      (TUnit,r)
    | Op s ->
      let r = match s with
        | "+" | "-" | "*" | "/" -> TFun(TPair(TInt,TInt), TInt)
        | "<" | ">" | "==" |">=" | "<=" -> TFun(TPair(TInt,TInt), TBool)
        | "&&" | "||" -> TFun(TPair(TBool,TBool), TBool)
        | _ -> failwith "Op unknow" in
      (r,env)

  (** Bonus : version polymorphe *)
end

module SubTypeChecker = struct


  (**
     Exercice 3 : sous-typage.

     On ajoute un type optionnel [T?] désignant des valeurs de type [T]
     éventuellement non définies (contrairement au type [T] lui-même pour
     lequel la valeur est à coup sûr définie.

     On a donc la relation de sous-typage [T <: T?], de laquelle on déduit
     une relation plus générale avec les règles habituelles.
  *)


  (**
     Parallèlement à l'introduction du type optionnel, on modifie l'opérateur
     de création de référence, qui crée une référence non initialisée sur un
     type [T] donné en paramètre.
     L'expression [newref T] aura donc le type [ref T?].

     On crée également un opérateur ["isNull"] testant si une valeur donnée
     est indéfinie.
  *)


  (**
     Vérification de types avec sous-typage.

     Ajouter du sous-typage au vérificateur de types simples, avec la règle
     algorithmique standard : le paramètre effectif d'un appel de fonction peut
     être un sous-type du type du paramètre formel.

     On ajoutera les particularités suivantes :
     - Tout opérateur travaillant sur des valeurs concrètes nécessitera des
       opérandes dont le type *n'est pas* un type optionnel.
     - Dans une expression de la forme [if isNull a then e₁ else e₂] avec [a] de
       type [T?], on pourra donner à [a] le type [T] dans la branche [e₂].
  *)



  open OptionTypes
  open SubAST
  (* EXO 1 *)

  module Env = Map.Make(String)
  type type_env = OptionTypes.typ Env.t

  let rec typecomp t1 t2 =
    match t1,t2 with
    | a,b when a=b -> true
    | a,TMaybe b when a=b -> true
    | _ -> false

  let rec type_expression (env: type_env) (e: expression) : typ =
    match e with
    | Int _ -> TInt
    | Bool _ -> TBool
    | Unit -> TUnit
    | Var s -> Env.find s env
    | App (e1,e2) ->
      let ty2 = type_expression env e2 in
      let ty1 = type_expression env e1 in
      let ty_arg, ty_res = match ty1 with
        | TFun(ty_a, ty_r) -> ty_a, ty_r
        | _ -> failwith "TFun expected"
      in
      if typecomp ty_arg ty2
      then ty_res
      else failwith "Incompatible application"
    | Fun (s,typ,e1) ->
      let env' = Env.add s typ env in
      let t_e = type_expression env' e1 in
      TFun(typ,t_e)
    | Let (s,e1,e2)    ->
      let typ_e1 = type_expression env e1 in
      let env' = Env.add s typ_e1 env in
      type_expression env' e2
    | Pair (e1,e2) -> TPair(type_expression env e1,type_expression env e2)
    | Fst e1 ->
      begin
        match type_expression env e1 with
        | TPair (e1,e2) -> e1
        | _ -> failwith "Fst need TPair"
      end
    | Snd e1 ->
      begin
        match type_expression env e1 with
        | TPair (e1,e2) -> e2
        | _ -> failwith "Fst need TPair"
      end
    | NewRef e1 -> TRef (TMaybe e1)
    | DeRef e1 ->
      begin
        match type_expression env e1 with
        | TRef r -> r
        | _ -> failwith "DeRef need TRef"
      end
    | Sequence (e1,e2) ->
      begin
        match type_expression env e1 with
        | TUnit -> type_expression env e2
        | _ -> failwith "First exp in Seq need Unit"
      end

    | If (IsNull(Var s),e1,e2) ->
      let typ_e = Env.find s env in
      begin
        match typ_e with
        | TMaybe a ->
          let new_env = Env.add s a env in
          let typ_e1 = type_expression env e1 in
          let typ_e2 = type_expression new_env e2 in
          if typ_e1 = typ_e2 then typ_e1 else failwith "TIf != TElse"
        | _ -> failwith "IsNull need TMaybe"
      end

    | If (ee,e1,e2) ->
      let typ_e = type_expression env ee in
      let typ_e1 = type_expression env e1 in
      let typ_e2 = type_expression env e2 in
      begin
        match typ_e,typ_e1,typ_e2 with
        | TBool,t1,t2 when t1 = t2 -> t1
        | _ -> failwith "If ee not TBool"
      end

    | While  (e1,e2) ->
      let typ_e1 = type_expression env e1 in
      let typ_e2 = type_expression env e2 in
      begin
        match typ_e1,typ_e2 with
        | TBool,TUnit -> TUnit
        | _ -> failwith"While Fail"
      end
    | IsNull e1 ->
      let typ_e1 = type_expression env e1 in
      begin
        match typ_e1 with
        | TMaybe _ -> TBool
        | _ -> failwith"IsNull Fail"
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

module Last = struct
  (**
     Exercice 4 : combiner l'inférence (exercice 2) et le sous-typage (exercice 3)
  *)
  open OptionTypes
  open LastAST


  module Env = Map.Make(String)
  type type_env = OptionTypes.typ Env.t


  let fresh_type_variable =
    let cpt = ref 64 in
    fun () -> if !cpt = 90 then cpt := 97 else incr cpt;
      Printf.sprintf "'%c" (Char.chr !cpt)



  let rec comp (t1:typ) (t2:typ) : bool =
    match t1,t2 with
    | TVar _ ,_ -> true
    | _ ,TVar _ -> true
    | TFun (t1,t2), TFun(t3,t4) -> comp t1 t3 && comp t2 t4
    | TPair (t1,t2), TPair (t3,t4) -> comp t1 t3 && comp t2 t4
    | TRef t1, TRef t2 -> comp t1 t2
    | r1,r2 -> r1 = r2


  let rec merge_vars k v1 v2 =
    (* Printf.printf "!!%s , %s\n" (printOption v1) (printOption v2); *)
    match v1, v2 with
    | None, None -> None
    | Some r, None | None, Some r -> Some r
    | Some (TVar _), Some r | Some r, Some (TVar _) -> Some r
    | Some r1, Some r2 when r1 = r2 -> Some r1
    | Some (TMaybe r1), Some r2 | Some r2, Some (TMaybe r1) ->
      begin
        match merge_vars k (Some r1) (Some r2) with
        | Some a -> Some (TMaybe a)
        | _ -> failwith "pas possible"
      end


    | _ -> failwith "2 type dif pour 1 var"

  let rec f (t1:OptionTypes.typ) (t2:OptionTypes.typ) (env:type_env) : type_env =
    (* let () = print_string (printtypO t1);print_string "\n";
      print_string (printtypO t2);print_string "\n" in *)
    match t1,t2 with
    | TVar a ,t -> Env.map (fun el -> if el = TVar a then t else el ) env
    | t ,TVar a -> env
    | TFun (t1,t2), TFun(t3,t4) -> Env.merge merge_vars (f t1 t3 env) (f t2 t4 env)
    | TPair (t1,t2), TPair (t3,t4) ->
      Env.merge merge_vars (f t1 t3 env) (f t2 t4 env)
    | TRef a, TRef b | TMaybe a, b | a, TMaybe b -> f a b env
    | r1,r2 ->
      if r1 = r2
      then env
      else raise (Etype (Printf.sprintf "Type Mismatch %s != %s"
                           (printtypO r1)
                           (printtypO r2)))

  let rec type_expression (env: type_env) (e: expression) : typ * type_env =
    match e with
    | Int _ -> (TInt,env)
    | Bool _ -> (TBool,env)
    | Unit -> (TUnit,env)
    | Var s -> (Env.find s env,env)


    | App (e1,e2) ->
      let (ty1,env1) = type_expression env e1 in
      let (ty2,env2) = type_expression env1 e2 in
      let ty_arg, ty_res = match ty1 with
        | TFun(ty_a, ty_r) -> ty_a, ty_r
        | _ -> failwith "TFun expected"
      in
      let new_env = f ty2 ty_arg env2 in
      (ty_res,new_env)




    | Fun (s,e1) ->
      let lab = fresh_type_variable () in
      let env' = Env.add s (TVar lab) env in
      let (t_e,env'') = type_expression env' e1 in
      let t1 = Env.find s env'' in
      (TFun(t1,t_e),Env.remove s env'')



    | Let (s,e1,e2) ->
      let (typ_e1,env1) = type_expression env e1 in
      let env' = Env.add s typ_e1 env1 in
      type_expression env' e2

    | Pair (e1,e2) ->
      let (typ_e1,env1) = type_expression env e1 in
      let (typ_e2,env2) = type_expression env1 e2 in
      (TPair(typ_e1,typ_e2),env2)

    | Fst e1 ->
      let (typ_e1,env1) = type_expression env e1 in
      begin
        match typ_e1 with
        | TPair (e1,_) -> (e1,env1)
        | TVar a ->
          let lab1 = fresh_type_variable () in
          let lab2 = fresh_type_variable () in
          (TVar lab1, f (TVar a) (TPair(TVar lab1,TVar lab2)) env1)
        | _ -> failwith "Fst need TPair"
      end

    | Snd e1 ->
      let (typ_e1,env1) = type_expression env e1 in
      begin
        match typ_e1 with
        | TPair (e1,e2) -> (e2,env1)
        | TVar a ->
          let lab1 = fresh_type_variable () in
          let lab2 = fresh_type_variable () in
          (TVar lab2,f (TVar a) (TPair(TVar lab1,TVar lab2)) env1)
        | _ -> failwith "Fst need TPair"
      end

    | NewRef e1 -> (TRef (TMaybe e1),env)

    | DeRef e1 ->
      let (typ_e,env') = type_expression env e1 in
      begin
        match typ_e with
        | TRef r -> (r,env')
        | TVar a ->
          let lab = fresh_type_variable () in
          (TVar lab, f (TVar a) (TRef (TVar lab)) env')
        | _ -> failwith "DeRef need TRef"
      end

    | Sequence (e1,e2) ->
      let (typ_e,env') = type_expression env e1 in
      begin
        match typ_e with
        | TUnit -> type_expression env' e2
        | TVar a ->
          let (typ_e2,env2) = type_expression env' e2 in
          (typ_e2, f (TVar a) TUnit env')
        | _ -> failwith "First exp in Seq need Unit"
      end

    | If (IsNull(Var s),e1,e2) ->
      let typ_e = Env.find s env in
      begin
        match typ_e with
        | TMaybe a ->
          let new_env = Env.add s a env in
          let (typ_e1,env1) = type_expression env e1 in
          let (typ_e2,env2) = type_expression new_env e2 in
          if typ_e1 = typ_e2
          then (typ_e1,env2)
          else failwith "TIf != TElse"


        | TVar a ->
          let lab = fresh_type_variable () in
          let (typ_e1,env1) = type_expression (Env.add s (TMaybe (TVar lab)) env) e1 in
          let (typ_e2,env2) = type_expression (Env.add s (TVar lab) env) e2 in

          let new_env = f typ_e1 typ_e2 (Env.merge merge_vars env2 env1) in
          (typ_e1,new_env)


        | _ -> failwith "IsNull need TMaybe"
      end

    | If (ee,e1,e2) ->
      let (typ_e,env') = type_expression env ee in
      let (typ_e1,env1) = type_expression env' e1 in
      let (typ_e2,env2) = type_expression env1 e2 in
      begin
        match typ_e,typ_e1,typ_e2 with
        | TBool,t1,t2 when t1 = t2 -> (t1,env2)
        | TVar a,t1,t2 when t1 = t2 -> (t1, f (TVar a) TBool env2)
        | _ -> failwith "If Fail"
      end

    | While  (e1,e2) ->
      let (typ_e1,env1) = type_expression env e1 in
      let (typ_e2,env2) = type_expression env1 e2 in
      let r = match typ_e1,typ_e2 with
        | TBool,TUnit -> env2
        | TVar a,TUnit -> f (TVar a) TBool env2
        | TBool, TVar a -> f (TVar a) TUnit env2
        | TVar a, TVar b -> Env.merge merge_vars
                              (f (TVar a) TBool env2)
                              (f (TVar b) TUnit env2)
        | _ -> failwith "While Fail"
      in
      (TUnit,r)

    | IsNull e1 ->
      let (typ_e1,env') = type_expression env e1 in
      begin
        match typ_e1 with
        | TMaybe _ -> (TBool,env')
        | _ -> failwith"IsNull Fail"
      end

    | Op s ->
      let r = match s with
        | "+" | "-" | "*" | "/" -> TFun(TPair(TInt,TInt), TInt)
        | "<" | ">" | "==" |">=" | "<=" -> TFun(TPair(TInt,TInt), TBool)
        | "&&" | "||" -> TFun(TPair(TBool,TBool), TBool)
        | _ -> failwith "Op unknow" in
      (r,env)



end

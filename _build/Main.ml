open Inference
open Type
open Printer


(* EXO 1 *)
let () = print_string "------EXO1------\n"


let a = BaseAST.App (BaseAST.Op "+", BaseAST.Pair ((BaseAST.Int 1),(BaseAST.Int 2)))
let env = BaseTypeChecker.Env.empty
let ex = BaseTypeChecker.type_expression env a
let () = Printf.printf "%s\n" (printtyp ex)


(* EXO 2 *)
let () = print_string "------EXO2------\n"


let f b =
  let env = BaseTypeReconstruction.Env.empty in
  let (ex,env) = BaseTypeReconstruction.type_expression env b in
  Printf.printf "%s\n" (printtyp ex)
(* printenv env *)

open RawAST

let () =

  (* Identity *)
  Fun("y",Var "y") |> f;

  (* Fst *)
  Fun("y",Fst (Var "y")) |> f;

  (* Snd *)
  Fun("y",Snd (Var "y")) |> f;

  (* Let *)
  Fun("y",
      Let (
        "x",
        App (
          Op "+",
          Pair ((Var "y"),(Var "y"))),
        App (
          Op "+",
          Pair ((Var "x"),(Var "x"))))) |> f;

  (* Op *)
  Fun("x",
      App (
        Op "+",
        Pair ((Var "x"),(Var "x")))) |> f;


  (* If *)
  Fun("x",
      If(
        Var "x",
        Int 10,
        Int 12
      )
     ) |> f;

  (* Seq *)
  Fun("x",
      Sequence(Var "x", Int 2)
     ) |> f;

  (* Ref *)
  Fun("x",
      DeRef(Var "x")
     ) |> f;

  (* Double Fun Avec While *)
  Fun(
    "x",
    Fun(
      "y",
      While(
        Var "x",
        Var "y"
      )
    )
  ) |> f;


  (* Erreur Les 2 coter de la Seq n'infer pas le meme type pour x*)
  begin
    try
      Fun(
        "x",
        Sequence(
          Fst(Var "x"),
          App (
            Op "+",
            Pair ((Var "x"),(Var "x")))    )
      ) |> f
    with
    | Etype s -> Printf.printf "%s\n" s
    | _ -> print_string "Erreur\n"
  end;


  (* env sort de fun *)
  begin
    try
      Pair(
        Fun(
          "x",
          App (
            Op "+",
            Pair ((Var "x"),(Var "x")))
        ),Var "x"


      ) |> f
    with
    | Etype s -> Printf.printf "%s\n" s
    | Not_found -> Printf.printf "Variable not fount\n"
    | _ -> print_string "Erreur\n"
  end;


  ()

(* EXO3 *)

let () = print_string "------EXO3------\n"

let f b =
  let env = SubTypeChecker.Env.empty in
  let ex = SubTypeChecker.type_expression env b in
  Printf.printf "%s\n" (printtypO ex)
(* printenv env *)

open SubAST
let () =

  (* If IsNull *)
  Fun("y",TMaybe TInt,
      If(
        IsNull (Var "y"),
        Int 10,
        App (
          Op "+",
          Pair ((Var "y"),(Var "y"))
        )
      )
     ) |> f;

  (* Fun need Int take ?Int *)
  App(Fun("x",TInt,Var "x"),DeRef (NewRef TInt)) |> f;

  App(
    Fun("y",TMaybe TInt,
        If(
          IsNull (Var "y"),
          Int 10,
          App (
            Op "+",
            Pair ((Var "y"),(Var "y"))
          )
        )
       ),
    DeRef (NewRef TInt)) |> f;
  ()


(* EXO4 *)

let () = print_string "------EXO4------\n"

let f b =
  let env = Last.Env.empty in
  let (ex,env) = Last.type_expression env b in
  Printf.printf "%s\n" (printtypO ex)

open LastAST

let () =

  Fun("y",
      If(
        IsNull (Var "y"),
        Int 10,
        App (
          Op "+",
          Pair ((Var "y"),(Var "y"))
        )
      )
     ) |> f;



  App(
    Fun("y",
        If(
          IsNull (Var "y"),
          Int 10,
          App (
            Op "+",
            Pair ((Var "y"),(Var "y"))
          )
        )
       ),
    DeRef (NewRef TInt)) |> f;


  ()

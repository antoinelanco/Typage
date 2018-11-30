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

module BaseAST = struct

  type expression =
    | Unit
    | Int    of int
    | Bool   of bool
    | Var    of string
    | App    of expression * expression
    | Fun    of string * SimpleTypes.typ * expression
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


module RawAST = struct
  open SimpleTypes

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

module OptionTypes = struct

  (** Les types simples + un type optionnel *)
  type typ =
    | TInt                (** Entiers [int]          *)
    | TBool               (** Booléens [bool]        *)
    | TUnit               (** Unité [unit]           *)
    | TVar of string      (** Variables de type ['a] *)
    | TFun  of typ * typ  (** Fonctions [T₁ ⟶ T₂]  *)
    | TPair of typ * typ  (** Paires [T₁ * T₂]       *)
    | TRef  of typ        (** Références [ref T]     *)
    | TMaybe of typ       (** Type option [T?]       *)

end


module SubAST = struct

  type expression =
    | Unit
    | Int    of int
    | Bool   of bool
    | Var    of string
    | App    of expression * expression
    | Fun    of string * OptionTypes.typ * expression
    | Let    of string * expression * expression
    | Op     of string
    | Pair   of expression * expression
    | Fst    of expression
    | Snd    of expression
    | NewRef of OptionTypes.typ
    | DeRef  of expression
    | Sequence of expression * expression
    | If     of expression * expression * expression
    | While  of expression * expression
    | IsNull of expression

end


module LastAST = struct

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
    | NewRef of OptionTypes.typ
    | DeRef  of expression
    | Sequence of expression * expression
    | If     of expression * expression * expression
    | While  of expression * expression
    | IsNull of expression

end

functor ExpandEnvFn
(
  structure Symbol : SYMBOL
  structure LexicalContext : LEXICAL_CONTEXT
  structure SymbolMap : SYMBOL_MAP
  sharing type Symbol.t
             = SymbolMap.Symbol.t
) : EXPAND_ENV =
struct


structure Symbol = Symbol
structure LexicalContext = LexicalContext

datatype 'a transform =
  Fun
| Quote
| If
| Begin
| Letrec
| LetSyntax
| Var of Symbol.t * LexicalContext.t
| Val of 'a 

(* 
* Combination of the mutable global scope and local scopes
* implemented with a list.
*)
type 'a t = ('a transform SymbolMap.t) * (Symbol.t * 'a transform) list

fun mkExpandEnv () = (SymbolMap.mkMap (), [])

fun fromList assocList = let
  val globalScope = SymbolMap.mkMap ()
in
  ( List.app (fn (sym, transform) =>
                SymbolMap.insert (globalScope, sym, transform))
             assocList
  ; (globalScope, []))
end

fun lookup ((globalScope, localScopes), sym) = let
  fun loop [] = NONE
    | loop ((sym', transform) :: rest) =
        if Symbol.=(sym, sym') then (
          SOME transform
        ) else (
          loop rest
        )
in
  case loop localScopes of
    NONE => SymbolMap.lookup (globalScope, sym)
  | transform as (SOME _) => transform
end

fun lookupGlobal ((globalScope, _), sym) =
  SymbolMap.lookup (globalScope, sym)

fun extend ((globalScope, localScopes), sym, transform) =
  (globalScope, (sym, transform) :: localScopes)

fun addGlobal ((globalScope, _), sym, transform) =
  SymbolMap.insert (globalScope, sym, transform)


end

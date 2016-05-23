functor ValueFn 
(
  structure SymbolTable : SYMBOL_TABLE
  structure LexicalContext : LEXICAL_CONTEXT
  structure ExpandEnv : EXPAND_ENV
  sharing type SymbolTable.Symbol.t 
             = LexicalContext.Name.t
             = ExpandEnv.Symbol.t
  sharing type LexicalContext.t
             = ExpandEnv.LexicalContext.t
) : VALUE =
struct


structure SymbolTable = SymbolTable
structure LexicalContext = LexicalContext
structure Ast = Ast
structure EvalEnv = EvalEnv
structure ExpandEnv = ExpandEnv

type srcloc =
  {name : string,
   line : int, column : int, position : int, span : int}

datatype t =
  Nil
| True
| False
| Void
| Undefined
| Int of int
| Char of char
| Str of string
| Sym of SymbolTable.Symbol.t
| Ref of t ref
| Pair of t * t
| FunFixArity of int * t EvalEnv.t * t Ast.t list
| FunVarArity of int * t EvalEnv.t * t Ast.t list
| FunBuiltInFixArity of SymbolTable.Symbol.t * int
      * (int * t ExpandEnv.t * LexicalContext.Mark.t * t EvalEnv.frame
      -> t OrError.t)
| FunBuiltInVarArity of SymbolTable.Symbol.t * int * int option
      * (int * t ExpandEnv.t * LexicalContext.Mark.t * t EvalEnv.frame
      -> t OrError.t)
| Stx of t * LexicalContext.t * (srcloc option)
| Error of t * (srcloc option)

fun stripStx (Stx (Pair (hd, tl), _, _)) = Pair (stripStx hd, stripStx tl)
  | stripStx (Pair (hd, tl)) = Pair (stripStx hd, stripStx tl)
  | stripStx (Stx (atom, _, _)) = stripStx atom
  | stripStx datum = datum

fun fromBool true = True
  | fromBool false = False

val >>= = OrError.>>=
infix >>=

fun ctxMap f (Stx(lst as Pair(_,_), ctx, loc)) =
      ctxMapList f lst >>= (fn lstResult =>
      OrError.return (Stx(lstResult, f ctx, loc)))
  | ctxMap f (Stx(atom, ctx, loc)) = 
      OrError.return (Stx(atom, f ctx, loc))
  | ctxMap _ _ = OrError.errorString "ValueFn.ctxMap: expected a syntax object"

and ctxMapList f Nil = OrError.return Nil
  | ctxMapList f (Pair(hd, tl)) = 
      ctxMapList f tl >>= (fn tlResult =>
      ctxMap f hd >>= (fn hdResult =>
      OrError.return (Pair(hdResult, tlResult))))
  | ctxMapList f (stx as Stx(_,_,_)) = ctxMap f stx
  | ctxMapList _ _ =
      OrError.errorString "ValueFn.ctxMap: expected a list or syntax object"


(********************** List Operations **********************)

fun car (Pair(hd, _)) = SOME hd
  | car _ = NONE
fun carExn (Pair(hd, _)) = hd
  | carExn _ = raise (Fail "carExn: argument is not a pair")

fun cdr (Pair(_, tl)) = SOME tl
  | cdr _ = NONE
fun cdrExn (Pair(_, tl)) = tl
  | cdrExn _ = raise (Fail "cdrExn: argument is not a pair")

fun length x = let
  fun loop (Nil, len) = len
    | loop (Pair(_, tl), len) = loop (tl, len + 1)
    | loop (_, len) = len + 1
in
  loop (x, 0)
end

fun toArray lst = let
  val array = Array.array (length lst, Undefined)
  fun loop (Nil, _) = ()
    | loop (Pair(hd, tl), i) =
        ( Array.update (array, i, hd)
        ; loop (tl, i + 1))
    | loop (atom, i) = Array.update (array, i, atom)
in
  loop (lst, 0)
  ; array
end

fun fromList [] = Nil
  | fromList (hd :: tl) = Pair(hd, fromList tl)

fun toList Nil = []
  | toList (Pair (hd, tl)) = hd :: (toList tl)
  | toList atom = [atom]

fun isList Nil = true
  | isList (Pair (_, tl)) = isList tl
  | isList _ = false

fun reverse lst = let
  fun loop (Nil, output) = output
    | loop (Pair (hd, tl), output) = loop (tl, Pair (hd, output))
    | loop (atom, output) = Pair (atom, output)
in
  loop (lst, Nil)
end

fun reversePrepend (Nil, base) = base
  | reversePrepend (Pair (hd, tl), base) = reversePrepend (tl, Pair (hd, base))
  | reversePrepend (atom, base) = Pair (atom, base)

fun map f Nil = Nil
  | map f (Pair(hd, tl)) = Pair(f hd, map f tl)
  | map f atom = f atom

fun foldl f init Nil = init
  | foldl f init (Pair (hd, tl)) = foldl f (f (hd, init)) tl
  | foldl f init other = f (other, init)

fun foldr f init Nil = init
  | foldr f init (Pair (hd, tl)) = f (hd, foldr f init tl)
  | foldr f init other = f (other, init)

fun all f Nil = true
  | all f (Pair (hd, tl)) = if (f hd) then all f tl else false
  | all f atom = f atom

fun any f Nil = false
  | any f (Pair (hd, tl)) = if (f hd) then true else any f tl
  | any f atom = f atom


end

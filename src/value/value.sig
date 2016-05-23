signature VALUE =
sig


structure SymbolTable : SYMBOL_TABLE
structure LexicalContext : LEXICAL_CONTEXT
structure Ast : AST
structure EvalEnv : EVAL_ENV
structure ExpandEnv : EXPAND_ENV
sharing type SymbolTable.Symbol.t 
           = LexicalContext.Name.t
           = ExpandEnv.Symbol.t
sharing type LexicalContext.t
           = ExpandEnv.LexicalContext.t
              

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

(* 
* Functions defined by lambda expressions capture their
* environment, store the arity of the lambda parameter list,
* and store the AST representation of the lambda body.
*
* FunFixArity (n, env, ast)
*   A function with arity = n.
*
* FunVarArity (n, env, ast)
*   A function with arity >= n.
*)
| FunFixArity of int * t EvalEnv.t * t Ast.t list
| FunVarArity of int * t EvalEnv.t * t Ast.t list

(* 
* Built-in functions are implemented by an SML function
* that take as inputs an environment frame containing the
* passed in arguments and a integer that the evaluator
* uses to pass the current phase. The phase number
* is important for built-in functions that need to manipulate
* the phase tower. The SML function can capture a reference to
* the phase tower and the passed in phase number can be used to 
* determine which phase(s) to manipulate.
*
* FunBuiltInFixArity (operationName, n, f)
*   A built-in function with arity = n.
*
* FunBuiltInVarArity (operationName, n, SOME m, f)
*   A built-in function with arity >= n and <= m.
*
* FunBuiltInVarArity (operationName, n, NONE, f)
*   A built-in function with arity >= n.
*) 
| FunBuiltInFixArity of SymbolTable.Symbol.t * int
      * (int * t ExpandEnv.t * LexicalContext.Mark.t * t EvalEnv.frame
      -> t OrError.t)
| FunBuiltInVarArity of SymbolTable.Symbol.t * int * int option
      * (int * t ExpandEnv.t * LexicalContext.Mark.t * t EvalEnv.frame
      -> t OrError.t)
| Stx of t * LexicalContext.t * (srcloc option)
(* 
* The type used for OrError style error handling.
* The source location is the place where the error value was
* created, which is set by the built-in (error ...) form.
*)
| Error of t * (srcloc option)

(* Recursively strips away syntax object wrappings *)
val stripStx : t -> t

val fromBool : bool -> t

(* Recursively maps lexical contexts to new lexical contexts *)
val ctxMap: (LexicalContext.t -> LexicalContext.t) -> t -> t OrError.t


(********************** List Operations **********************)

(* car and cdr return NONE if the input is not a pair. *)
val car : t -> t option
val cdr : t -> t option

(* 
* carExn and cdrExn raise (Fail "argument is not a pair") if
* input is not a pair.
*)
val carExn : t -> t
val cdrExn : t -> t

(*
* Converts proper and dotted lists to an array of their elements.
*)
val toArray : t -> t array

(*
* Returns length of proper lists and dotted lists.
* length (v0 ... vN) = N
* length (v0 ... vN . vN+1) = N+1
*)
val length : t -> int

(*
* Create a value list from an SML list
*)
val fromList : t list -> t

val toList : t -> t list

(* 
* Returns true if the value is made up of a sequence of Pairs
* ending in Nil: 'Pair(_, ... Pair(_, Nil))'.
*)
val isList : t -> bool

(*
* Returns a list that contains the values in the input list
* in reverse. A dotted list is NOT returned for a dotted list
* input, the final item of the input dotted list is the first element
* in the returned proper list.
*)
val reverse : t -> t

(*
* Prepends the first argument list to the second argument in reverse
* order. If the second element is not a proper list, then the returned
* list is also not a proper list.
*)
val reversePrepend : t * t -> t

(* 
* For call of 'map f datum':
* If the datum is Nil: returns Nil.
* If the datum is 'Pair (hd, tl)':
*   returns 'Pair(f hd, map f tl)'.
* If the datum is any other value: returns 'f datum'.
*)
val map : (t -> t) -> t -> t


(* 
* For call of 'foldl f init datum':
* If the datum is Nil: returns 'init'.
* If the datum is 'Pair (hd, tl)':
*   returns 'foldl f (f (hd, init)) tl'.
* If the datum is any other value: returns 'f (datum, init)'.
*)
val foldl : (t * 'a -> 'a) -> 'a -> t -> 'a

(*
* For call of 'foldr f init datum':
* If the datum is Nil: returns 'init'.
* If the datum is 'Pair (hd, tl)':
*   returns 'f (hd, foldr f init tl)'.
* If the datum is any other value: returns 'f (datum, init)'.
*)
val foldr : (t * 'a -> 'a) -> 'a -> t -> 'a

(*
* The call (all f lst) returns true if 'f' returns true when called
* on all of the elements of the list or dotted list. The function
* short circuits, returning immediately if a call to 'f' return false.
*)
val all : (t -> bool) -> t -> bool

(*
* The call (any f lst) returns true if 'f' returns true when called
* on any of the elements of the list or dotted list. The function
* short circuits, returning immediately if a call to 'f' return true.
*)
val any : (t -> bool) -> t -> bool


end

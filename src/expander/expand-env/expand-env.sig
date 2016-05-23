signature EXPAND_ENV =
sig


structure Symbol : SYMBOL
structure LexicalContext : LEXICAL_CONTEXT

type 'a t

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
* Create an empty expand environment.
*)
val mkExpandEnv : unit -> 'a t

(*
* Create an expand environment with the intial 
* contents of the global scope described by the input list.
*)
val fromList : (Symbol.t * 'a transform) list -> 'a t

(*
* Lookup up a symbol lexically. Searches the nested
* scopes all the way up to the global scope.
* Returns NONE if the symbol is not mapped to anything in
* the environment.
*)
val lookup : 'a t * Symbol.t -> 'a transform option

(* 
* Look up a symbol only in the global scope. Returns NONE
* if the symbol is not mapped to anything in the environment.
*)
val lookupGlobal : 'a t * Symbol.t -> 'a transform option

(* Extend the environment locally. *)
val extend : 'a t * Symbol.t * 'a transform -> 'a t

(* Add a transform to the global scope. *)
val addGlobal : 'a t * Symbol.t * 'a transform -> unit


end

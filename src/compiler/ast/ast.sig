signature AST =
sig


datatype 'a t =
  Constant of 'a
| ShallowRef of int
| DeepRef of int * int
| GlobalRef of int
| If of 'a t * 'a t * 'a t
| Begin of 'a t list
(* Function with arity = n. *)
| FunFixArity of int * 'a t list
(* Function with arity >= n. *)
| FunVarArity of int * 'a t list
| Letrec of 'a t vector * 'a t list
(* 
* Arguments 'a to let and app are stored in reverse
* order 'a to avoid having 'a to waste work reversing lists when
* operating on 'a the arguments with 'a tail recursive functions
*)
| Let of 'a t vector * 'a t list
| App of 'a t * 'a t vector

(* Given a function that prints constants, print an AST. *)
val print : TextIO.outstream * (TextIO.outstream * 'a -> unit) * 'a t -> unit


end

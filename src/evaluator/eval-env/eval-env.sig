signature EVAL_ENV =
sig
  
  
type 'a t
type 'a frame = 'a array

val mkEmpty : unit -> 'a t

val mkFromGlobalTable : 'a vector -> 'a t

val extend : 'a t * 'a frame -> 'a t

val shallowRef : 'a t * int -> 'a OrError.t

val deepRef : 'a t * int * int -> 'a OrError.t

val globalRef : 'a t * int -> 'a OrError.t

val print : 'a t * TextIO.outstream * ('a -> string) -> unit


end

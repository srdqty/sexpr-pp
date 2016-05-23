signature COMPILE_ENV =
sig


structure Symbol : SYMBOL

type t

datatype id_address =
  Free
| Shallow of int
| Deep of int * int

val lookup : t * Symbol.t -> id_address

val extend : t * Symbol.t list -> t

val empty : t


end

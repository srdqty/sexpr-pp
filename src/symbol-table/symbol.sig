signature SYMBOL =
sig


type t

(* O(1) equal operation. *)
val = : t * t -> bool

(*
* O(1) ordering operation.
* This is *NOT* based on string comparison.
* To compare the strings of a symbol, use
* toString and compare the returned strings.
* This operation is useful for adding symbols
* to set or map data structures that require
* an ordering.
*)
val compare : t * t -> order

(* O(1) hash operation *)
val hash : t -> Word32.word

(* O(1) converstion to string. *)
val toString : t -> string

(* 
* Converstion from string.
*  O(n) on length of string (hashing).
*)
val fromString : string -> t

(* 
* Create a unique symbol with a prefix equal to 
*  the string argument.
*)
val freshSym : string -> t

(* 
* Pre-created empty symbol that can be used when a default
*  value is necessary.
*)
val empty : t


end

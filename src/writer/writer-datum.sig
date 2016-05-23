signature WRITER_DATUM =
sig


type t

val isNil : t -> bool
val isAtom : t -> bool
val isPair : t -> bool
val isSyntax : t -> bool

val car : t -> t
val cdr : t -> t
val unWrapStx : t -> t

val displayAtom : t * (string -> unit) -> unit
val writeAtom : t * (string -> unit) -> unit

(* A function that can output a syntax object using a function
   that can write strings and another function that can write
   datums other than syntax objects *)
val writeSyntax : 
  t * { writeString : string -> unit, writeDatum : t -> unit } -> unit

val writeRenamedSyntax : 
  t * { writeString : string -> unit, writeDatum : t -> unit } -> unit

end

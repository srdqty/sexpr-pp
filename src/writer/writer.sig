signature WRITER =
sig


type datum

(* outputs a datum given a function that will output strings *)
val write : (string -> unit) -> datum -> unit

val display : (string -> unit) -> datum -> unit

val writeRenamedSyntax : (string -> unit) -> datum -> unit

val displayVerbose : (string -> unit) -> datum -> unit

end

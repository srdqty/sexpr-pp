signature READER_DATUM =
sig
  type t
  type srcloc = 
    {name : string,
     line : int, column : int, position : int, span : int}

  val Nil : t
  val Pair : t * t -> t
  val wrapWithSourceInfo : t * srcloc -> t
  val car : t -> t
  val cdr : t -> t
  val isNil : t -> bool
  val isPair : t -> bool
  val isSym : t -> bool
  val isSymEq : t * t -> bool

  val parseNumBoolOrSym : 
    string * {raiseError : string -> t, internSymbol : string -> t} -> t

  val parseCharConstant : string * (string -> t) -> t
  val parseStringLiteral : string * (string -> t) -> t
end


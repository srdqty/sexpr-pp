signature DATUM_CONSTRUCTOR =
sig
  type datum
  type symbol_table

  type srcloc = 
    {name : string,
     line : int, column : int, position : int, span : int}

  val makeNil : unit -> datum
  val makePair : datum * datum -> datum
  
  val wrapWithSourceInfo : datum * srcloc -> datum

  val isNil : datum -> bool
  val isPair : datum -> bool

  val parseNumBoolOrSym : string * symbol_table * srcloc -> datum OrError.t
  val parseCharConstant : string * srcloc -> datum OrError.t
  val parseStringLiteral : string * srcloc -> datum OrError.t
end

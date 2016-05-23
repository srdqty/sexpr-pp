signature READER =
sig
  structure DatumConstructor : DATUM_CONSTRUCTOR
  structure Lexer : LEXER

  (* Returns NONE at end of input stream *)

  val read : 
    DatumConstructor.symbol_table ->
    Lexer.instream ->
    DatumConstructor.datum option OrError.t

  val readSyntax : 
    DatumConstructor.symbol_table ->
    Lexer.instream ->
    DatumConstructor.datum option OrError.t
end

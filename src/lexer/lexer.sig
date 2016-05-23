signature LEXER =
sig
  type instream

  type srcloc = 
  { name : string, column : int, line : int, position : int, span : int }

  datatype token =
    End of srcloc
  | Lparen of srcloc
  | Rparen of srcloc
  | Dot of srcloc
  | ExprComment of srcloc
  | Char of string * srcloc
  | Str of string * srcloc
  | NumBoolOrSym of string * srcloc

  val mkLexInstream : TextIO.StreamIO.instream * string -> instream
  val getInstream : instream -> TextIO.StreamIO.instream
  val readToken : instream -> token OrError.t
  val srcloc : token -> srcloc
  val tokenToString : token -> string
end

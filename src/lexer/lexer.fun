structure Lexer :> LEXER =
struct
  type instream = TextInputPort.port ref

  type  srcloc = 
  { name : string, line : int, column : int, position : int, span : int }

  datatype token =
    End of srcloc
  | Lparen of srcloc
  | Rparen of srcloc
  | Dot of srcloc
  | ExprComment of srcloc
  | Char of string * srcloc
  | Str of string * srcloc
  | NumBoolOrSym of string * srcloc
  
  datatype lex_error =
    IllegalCharacter of 
    { (*stream    : TextIO.StreamIO.instream*)
     message   : string
    , character : char
    , srcloc    : srcloc
    }
  | IncompleteToken of
    { (*stream  : TextIO.StreamIO.instream*)
     message : string
    , srcloc  : srcloc
    }
  | IncompleteNestedComment of
    { (*stream  : TextIO.StreamIO.instream*)
     message : string
    , srcloc  : srcloc
    }
  
  fun mkLexInstream(instream, name) = 
    ref (TextInputPort.mkPort(instream, name))

  fun getInstream(lexInstream) = TextInputPort.getInstream(!lexInstream)


  fun srcloc(End srcloc) = srcloc
    | srcloc(Lparen srcloc) = srcloc
    | srcloc(Rparen srcloc) = srcloc
    | srcloc(Dot srcloc) = srcloc
    | srcloc(ExprComment srcloc) = srcloc
    | srcloc(Char(_, srcloc)) = srcloc
    | srcloc(Str(_, srcloc)) = srcloc
    | srcloc(NumBoolOrSym(_, srcloc)) = srcloc

  fun portsrc(beginPort, endPort) = 
    { name = TextInputPort.name beginPort, 
      line = TextInputPort.line beginPort,
      column = TextInputPort.column beginPort,
      position = TextInputPort.position beginPort,
      span = TextInputPort.span(beginPort, endPort) }
  
  fun locToString (name, line, column) =
    name
    ^ ":" ^ (Int.toString line)
    ^ ":" ^ (Int.toString column)
    ^ ": "

  fun illegalCharError(msg, c, port) =
    let
      val name = TextInputPort.name port
      val line = TextInputPort.line port
      val column = TextInputPort.column port
    in
      OrError.errorThunk
        (fn () =>
          (locToString (name, line, column)) ^ msg 
          ^ " '" ^ (Char.toString c) ^ "' " 
          ^ "(code " ^ (Int.toString (Char.ord c)) ^ ")")
    end

  fun incompleteTokenError(msg, port) =
    let
      val name = TextInputPort.name port
      val line = TextInputPort.line port
      val column = TextInputPort.column port
    in
      OrError.errorThunk
        (fn () =>
          (locToString (name, line, column)) ^ msg)
    end

  fun nestedCommentEarlyEnd(port) =
    let
      val name = TextInputPort.name port
      val line = TextInputPort.line port
      val column = TextInputPort.column port
    in
      OrError.errorThunk
        (fn () =>
          (locToString (name, line, column)) ^ "end of file in #| comment")
    end

  fun isWhitespace #" " = true
    | isWhitespace #"\n" = true
    | isWhitespace #"\t" = true
    | isWhitespace #"\r" = true
    | isWhitespace _ = false

  fun isDelimiter #"(" = true
    | isDelimiter #")" = true
    | isDelimiter #";" = true
    | isDelimiter #"\"" = true
    | isDelimiter c = isWhitespace c

  fun isReserved #"{" = true
    | isReserved #"}" = true
    | isReserved #"[" = true
    | isReserved #"]" = true
    | isReserved #"'" = true
    | isReserved #"`" = true
    | isReserved #"," = true
    | isReserved _ = false

  fun isLegal c = 
    (not (isReserved c)) 
    andalso (Char.ord c) > 32
    andalso (Char.ord c) < 127 

  fun readToken(lexStream) = let
    fun start(port) =
      case TextInputPort.input1 port of
        NONE => OrError.return (End(portsrc(port, port)), port)
      | SOME(#"(", port') => OrError.return (Lparen(portsrc(port, port')), port')
      | SOME(#")", port') => OrError.return (Rparen(portsrc(port, port')), port')
     
      | SOME(#"#", port') => scanNumberSign(port, port')

      | SOME(#".", port') => scanDot(port, port')

      | SOME(#";", port') => skipLineComment port'

      | SOME(#"\"", port') => scanString(port, port')
      
      | SOME(c, port') =>
        if isWhitespace c then skipWhitespace port'
        else if isLegal c then scanNumBoolOrSym(port, port')
        else illegalCharError("illegal character", c, port)
    
    and scanDot(beginPort, currPort) =
      case TextInputPort.input1 currPort of
        NONE => OrError.return (Dot(portsrc(beginPort, currPort)), currPort)
      | SOME(c, port') =>
        if isDelimiter c then OrError.return (Dot(portsrc(beginPort, currPort)), currPort)
        else if isLegal c then scanNumBoolOrSym(beginPort, port')
        else illegalCharError("ilegal character following '.'", c, currPort)
     
    and scanNumBoolOrSym(beginPort, currPort) =
      case TextInputPort.input1 currPort of
        NONE => 
        OrError.return (NumBoolOrSym(TextInputPort.inputSpan(beginPort,currPort),
                      portsrc(beginPort, currPort)),
         currPort)
      | SOME(c, port') =>
        if isDelimiter c then 
          OrError.return (NumBoolOrSym(TextInputPort.inputSpan(beginPort,currPort),
                        portsrc(beginPort, currPort)),
          currPort)
        else if isLegal c then scanNumBoolOrSym(beginPort, port')
        else illegalCharError("ilegal character in symbol", c, currPort)

    and scanNumberSign(beginPort, currPort) =
      case TextInputPort.input1 currPort of
        NONE => 
        OrError.return (NumBoolOrSym(TextInputPort.inputSpan(beginPort,currPort),
                      portsrc(beginPort, currPort)),
         currPort)
      | SOME(#"\\", port') => scanChar(beginPort, port')
      | SOME(#";", port') => 
        OrError.return (ExprComment(portsrc(beginPort, port')), port')
      | SOME(#"|", port') => 
        skipNestedComment(Stack.push(Stack.empty, beginPort), port', 0)
      | SOME(c, port') =>
        if isDelimiter c then 
          OrError.return (NumBoolOrSym(TextInputPort.inputSpan(beginPort,currPort),
                        portsrc(beginPort, currPort)),
          currPort)
        else if isLegal c then scanNumBoolOrSym(beginPort, port')
        else illegalCharError("ilegal character following '#'", c, currPort)

    and scanChar(beginPort, currPort) =
      case TextInputPort.input1 currPort of
        NONE => 
        incompleteTokenError("end of input before end of character literal",
                             beginPort)
      | SOME(_, port') => scanCharRest(beginPort, port')

    and scanCharRest(beginPort, currPort) =
      case TextInputPort.input1 currPort of
        NONE => OrError.return (Char(TextInputPort.inputSpan(beginPort, currPort), 
                      portsrc(beginPort, currPort)),
                 currPort)
      | SOME(c, port') =>
        if isDelimiter c then 
          OrError.return (Char(TextInputPort.inputSpan(beginPort, currPort), 
                portsrc(beginPort, currPort)),
           currPort)
        else scanCharRest(beginPort, port')

    and scanString(beginPort, currPort) =
      case TextInputPort.input1 currPort of
        NONE =>
        incompleteTokenError("end of input before end of string literal",
                             beginPort)
      | SOME(#"\\", port') => scanEscapedChar(beginPort, port')
      | SOME(#"\"", port') =>
        OrError.return (Str(TextInputPort.inputSpan(beginPort, port'),
             portsrc(beginPort, port')),
         port')
      | SOME(_, port') => scanString(beginPort, port')

    and scanEscapedChar(beginPort, currPort) =
      case TextInputPort.input1 currPort of
        NONE =>
        incompleteTokenError("end of input before end of string literal",
                             beginPort)
      (* Just pass the character on to the client of the lexer, this
         is just needed to skip over escaped quotation marks *)
      | SOME(_, port') => scanString(beginPort, port')

    and skipNestedComment(beginPortStack, currPort, level) =
      case TextInputPort.input1 currPort of
        NONE => nestedCommentEarlyEnd(Stack.top beginPortStack)
      | SOME(#"#", port') =>
        checkAnotherComment(beginPortStack, currPort, port', level)
      | SOME(#"|", port') =>
        checkEndComment(beginPortStack, port', level)
      | SOME(_, port') => skipNestedComment(beginPortStack, port', level)

    and checkAnotherComment(beginPortStack, newBeginPort, currPort, level) =
      case TextInputPort.input1 currPort of
        NONE => nestedCommentEarlyEnd(Stack.top beginPortStack)
      | SOME(#"|", port') => 
        skipNestedComment(Stack.push(beginPortStack, newBeginPort),
                          port', level + 1)
      | SOME(_, port') => skipNestedComment(beginPortStack, port', level)
    
    and checkEndComment(beginPortStack, currPort, level) =
      case TextInputPort.input1 currPort of
        NONE => nestedCommentEarlyEnd(Stack.top beginPortStack)
      | SOME(#"#", port') =>
        if level > 0
        then skipNestedComment(Stack.drop beginPortStack, port', level-1)
        else start(port')
      | SOME(_, port') => skipNestedComment(beginPortStack, port', level)

    and skipWhitespace port =
      case TextInputPort.input1 port of
        NONE => start port
      | SOME(c, port') => 
        if isWhitespace c then skipWhitespace port' else start port

    and skipLineComment port =
      case TextInputPort.input1 port of
        NONE => start port
      | SOME(#"\n", port') => start port'
      | SOME(_, port') => skipLineComment port'

    val >>= = OrError.>>=
    infix >>=
  in
    start (!lexStream) >>= (fn (tok, port) =>
    ( lexStream := port
    ; OrError.return tok
    ))
  end
  
fun tokenToString (End _) = "<End of Input>"
  | tokenToString (Lparen _) = "("
  | tokenToString (Rparen _) = ")"
  | tokenToString (Dot _) = "."
  | tokenToString (ExprComment _) = "#;"
  | tokenToString (Char(c, _)) = "Char(" ^ c ^ ")"
  | tokenToString (Str(s, srcloc)) = "Str(" ^ (String.toString s) ^ ")"
  | tokenToString (NumBoolOrSym(a, srcloc)) = "NumBoolOrSym(" ^ a ^ ")"

end

functor ReaderFn
(
  structure DatumConstructor : DATUM_CONSTRUCTOR
  structure Lexer : LEXER
) : READER =
struct
  structure DatumConstructor = DatumConstructor
  structure Lexer = Lexer

  structure DC = DatumConstructor

  fun combineLoc(loc1, loc2) = 
      let
        val name = #name loc1
        val line = #line loc1
        val col = #column loc1
        val pos = #position loc1
        val pos2 = #position loc2
        val span2 = #span loc2
        val span = pos2 - pos + span2
      in
        {name = name, line = line,
         column = col, position = pos, span = span }
      end

  fun reader(lexStream, stxWrap, symTable) = let
    fun nextToken() = Lexer.readToken lexStream

    fun span0Loc {name, line, column, position, span} =
      {name = name, line = line, column = column, 
       position = position, span = 0}

    fun unexpectedTokenErrorMsg tok =
      "unexpected token '" ^
      (Lexer.tokenToString tok) ^ "'"

    fun exprCommentErrorMsg _ = 
      "expected a commented out element for '#;'"

    fun illegalDotErrorMsg _ = "illegal use of '.'"

    fun closeRparenErrorMsg _ =
        "expected a ')' to close '('"

    fun readError {name, line, column, ...} makeMsg tok =
      OrError.errorThunk
        (fn () =>
          name 
          ^ ":" ^ (Int.toString line)
          ^ ":" ^ (Int.toString column)
          ^ ": " ^ (makeMsg tok))

    val isPair = DC.isPair
    val isNil = DC.isNil
    val cons = DC.makePair

    fun isRparen (Lexer.Rparen _) = true
      | isRparen _ = false

    val >>= = OrError.>>=
    infix >>=

    fun top token =
      case token of
        Lexer.End _ => OrError.return NONE
      | Lexer.ExprComment loc =>
          nextToken() >>= (fn tok =>
          expr(tok, exprCommentErrorMsg, loc) >>= (fn _ =>
          nextToken() >>= (fn tok =>
          top tok)))
      | token =>
          expr(token, unexpectedTokenErrorMsg, Lexer.srcloc token) >>=
          (fn datum => OrError.return (SOME datum))
  
    and expr(token, errorMsg, loc) =
      case token of
        tok as Lexer.End _ => (readError loc errorMsg tok)
      | Lexer.ExprComment loc' =>
          nextToken() >>= (fn tok =>
          expr(tok, exprCommentErrorMsg, loc') >>= (fn _ =>
          nextToken() >>= (fn tok =>
          expr(tok, errorMsg, loc))))
      | Lexer.Lparen lloc =>
          nextToken() >>= (fn tok =>
          listTail(tok, closeRparenErrorMsg, lloc, isRparen)
          >>= (fn (rloc, lst) =>
            if isPair lst orelse isNil lst
            then OrError.return (stxWrap(lst, combineLoc(lloc, rloc)))
            else OrError.return lst))
      | token => atom(token, errorMsg, loc)

    and listTail(token, errorMsg, loc, isEnder) =
      case token of
        tok as (Lexer.End _) => (readError loc errorMsg tok)
      | Lexer.Dot loc' =>
          nextToken() >>= (fn tok =>
          expr(tok, illegalDotErrorMsg, loc') >>= (fn item =>
          nextToken() >>= (fn tok =>
          dotTail(tok, illegalDotErrorMsg, loc', isEnder) >>= (fn rloc =>
          OrError.return (rloc, item)))))
      | Lexer.ExprComment loc' =>
          nextToken() >>= (fn tok =>
          expr(tok, exprCommentErrorMsg, loc') >>= (fn _ =>
          nextToken() >>= (fn tok =>
          listTail(tok, errorMsg, loc, isEnder))))
      | token =>
        if isEnder token
        then OrError.return (Lexer.srcloc token, DC.makeNil())
        else
          expr(token, errorMsg, loc) >>= (fn hd =>
          nextToken() >>= (fn tok =>
          listTail(tok, errorMsg, loc, isEnder) >>= (fn (rloc, tl) =>
          OrError.return (rloc, cons(hd, tl)))))

    and dotTail(token, errorMsg, loc, isEnder) =
      case token of
        tok as (Lexer.End _) => (readError loc errorMsg tok)
      | Lexer.ExprComment loc' =>
          nextToken() >>= (fn tok =>
          expr(tok, exprCommentErrorMsg, loc') >>= (fn _ =>
          nextToken() >>= (fn tok =>
          dotTail(tok, errorMsg, loc, isEnder))))
      | token =>
        if isEnder token 
        then OrError.return (Lexer.srcloc token)
        else (readError loc errorMsg token)

    and atom(token, errorMsg, loc) =
      case token of
        Lexer.Str(str, loc') =>
          DC.parseStringLiteral(str, loc') >>= (fn strLit =>
          OrError.return (stxWrap(strLit, loc')))
      | Lexer.Char(str, loc') =>
          DC.parseCharConstant(str, loc') >>= (fn charLit =>
          OrError.return (stxWrap(charLit, loc')))
      | Lexer.NumBoolOrSym(str, loc') =>
          DC.parseNumBoolOrSym(str, symTable, loc') >>= (fn atom =>
          OrError.return (stxWrap(atom, loc')))
      | token as Lexer.Dot loc' => 
        (readError loc' illegalDotErrorMsg token)
      | token => (readError loc errorMsg token)

  in
    nextToken() >>= (fn tok =>
    top tok)
  end

  fun readSyntax symTable stream =
        reader(stream, DC.wrapWithSourceInfo, symTable)

  fun read symTable stream =
        reader(stream, fn (x, y) => x, symTable)
end

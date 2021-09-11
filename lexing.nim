import lexer, language
export lexer

# todo: we can store current 'repr' not as separate string sequences, but range of source
#       this way we can get rid of lots of memory allocs/reallocs
#       --  somewhat resolved by futureSlice, tho it still copies the string,
#           ideally it should be pointer to a source where repr begins and its size or pointer to the end

# todo: should tkValue be ambiguous? with that it is necessary to re-parse tokens in the future
#       to get the actual typed value, we always could implement tokens as tagged unions of course
#       and store hint for future after lexing

# todo: specify descriptive error text on erroneous lex return for future displaying

# todo: make 'repr' of tokens as optional value of 'tvString'

# todo: make use of parseutils

# todo: do we need to keep info about spaces? or guess it from context

# todo: make export maker its own token for ease of parsing


# todo: make macro from this
func lexAny(p: Lexer, list: varargs[proc(p: Lexer): Token {.nimcall, gcsafe, noSideEffect.}]): Token {.inline.} =
  for fn in list:
    result = p.fn
    if result.valid: break


# todo: works, but not sure about that, could be clearer
func lexIdent(p: Lexer): Token =
  if p.current.isLetter:
    var future: int
    for ch in p.stream:
      if ch.isLetter or ch.isNumber:
        future.inc
      else:
        if ch == ExportMarker:
          future.inc
        break
    p.viewToken(tkIdent, future)
    # Token(kind: tkIdent, repr: p.futureSlice(future))
  else:
    Token(kind: tkError)


func lexKeyword(p: Lexer): Token =
  var future: int
  for ch in p.stream:
    if not ch.isLetter: break
    future.inc
  for keyword in ReservedWords:
    # if p.view(future) == keyword.toOpenArray(keyword.low, keyword.high):
    if toOpenArray(p.source, p.cursor, p.cursor + future) == keyword.toOpenArray(keyword.low, keyword.high):
      return Token(kind: tkKeyword)
  Token(kind: tkError)


func lexString(p: Lexer): Token =
  if p.current in StringMarkers:
    var future = 1
    for cur in p.stream(p.nextCursor):
      if cur == EndChar: break # no enclosing marker encountered
      future.inc
      if cur in StringMarkers:
        # return Token(kind: tkValue, repr: p.futureSlice(future))
        return p.viewToken(tkValue, future)
  Token(kind: tkError)


func lexInt(p: Lexer): Token =
  if p.current.isNumber:
    var future: int
    for ch in p.stream:
      if ch.isNumber:
        future.inc
      else: break
    p.viewToken(tkValue, future)
    # Token(kind: tkValue, repr: p.futureSlice(future))
  else:
    Token(kind: tkError)


# todo: maybe create unified definition for all such symbols?
func lexSpecialChars(p: Lexer): Token =
  case p.current:
  of '.':
    Token(kind: tkDot)
  of '=':
    Token(kind: tkAssign)
  of ':':
    Token(kind: tkColon)
  of '[':
    Token(kind: tkListOpen)
  of ']':
    Token(kind: tkListClose)
  else:
    Token(kind: tkError)


func lexValue(p: Lexer): Token =
  p.lexAny(lexInt, lexString)


func lexModule*(p: var Lexer) =
  while not p.atEnd:
    let tok = p.lexAny(lexKeyword, lexIdent, lexValue, lexSpecialChars)
    if tok.valid:
      p.eatToken tok
      p.eatSpace
    else:
      raise newException(LexerError, "unknown token")

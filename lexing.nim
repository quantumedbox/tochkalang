import lex

# todo: we can store current 'repr' not as separate string sequences, but range of source
#       this way we can get rid of lots of memory allocs/reallocs
#       --  somewhat resolved by futureSlice, tho it still copies the string,
#           ideally it should be pointer to a source where repr begins and its size or pointer to the end

# todo: should tkValue be ambiguous? with that it is necessary to re-parse tokens in the future
#       to get the actual typed value, we always could implement tokens as tagged unions of course
#       and store hint for future after lexing

# todo: specify errors on erroneous lex return for future displaying

# todo: make 'repr' of tokens as optional value of 'tvString'

# todo: project is strangely structured, there should be a file for language definitions,
#       another for lexer utils and another for lex parsing

const
  Keywords = ["proc", "scope", "mut", "type", "cond"] # should only consist of isLetter chars
  ExportMarker = '*'
  StringMarkers = {'"'}


# todo: make macro from this
proc lexAny(p: Lexer, list: varargs[proc(p: Lexer): Token {.nimcall, gcsafe.}]): Token {.inline.} =
  for fn in list:
    result = p.fn
    if result.valid: break


# todo: works, but not sure about that, could be clearer
proc lexIdent(p: Lexer): Token =
  if p.current.isLetter:
    var future: int
    for ch in p.stream:
      if ch.isLetter or ch.isNumber:
        future.inc
      else:
        if ch == ExportMarker:
          future.inc
        break
    Token(kind: tkIdent, repr: p.futureSlice(future))
  else:
    Token(kind: tkError)


proc lexKeyword(p: Lexer): Token =
  var future: int
  for ch in p.stream:
    if not ch.isLetter: break
    future.inc
  let parsed = p.futureSlice(future)
  for keyword in Keywords:
    if keyword == parsed:
      return Token(kind: tkKeyword, repr: parsed)
  Token(kind: tkError)


proc lexString(p: Lexer): Token =
  if p.current in StringMarkers:
    var future = 1
    for cur in p.stream(p.nextCursor):
      if cur == EndChar: break # no enclosing marker encountered
      future.inc
      if cur in StringMarkers:
        return Token(kind: tkValue, repr: p.futureSlice(future))
  Token(kind: tkError)


proc lexInt(p: Lexer): Token =
  if p.current.isNumber:
    var future: int
    for ch in p.stream:
      if ch.isNumber:
        future.inc
      else: break
    Token(kind: tkValue, repr: p.futureSlice(future))
  else:
    Token(kind: tkError)


proc lexSpecialChars(p: Lexer): Token =
  case p.current:
  of '=':
    Token(kind: tkAssign)
  of ':':
    Token(kind: tkColon)
  else:
    Token(kind: tkError)


proc lexValue(p: Lexer): Token =
  p.lexAny(lexInt, lexString)


proc lexModule(p: var Lexer) =
  while not p.atEnd:
    let tok = p.lexAny(lexKeyword, lexIdent, lexValue, lexSpecialChars)
    if tok.valid:
      p.eatToken tok
      p.eatSpace
    else:
      raise newException(LexerError, "unknown token")


let src = stdin.readAll
var p = initLexer(src, lexModule)
if p.tokenize:
  echo $p

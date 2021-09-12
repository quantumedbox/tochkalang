import lexer
export lexer

# todo: should tkValue be ambiguous? with that it is necessary to re-parse tokens in the future
#       to get the actual typed value, we always could implement tokens as tagged unions of course
#       and store hint for future after lexing

# todo: specify descriptive error text on erroneous lex return for future displaying

# todo: do we need to keep info about spaces? or guess it from context

# todo: make export maker its own token for ease of parsing

# todo: maybe it is possible to parse idents and keywords at the same func?


# todo: make macro from this
func lexAny(x: Lexer, list: varargs[LexerFunc]): LexerReturn {.inline.} =
  for fn in list:
    result = x.fn
    if result.tok.valid: break


# todo: works, but not sure about that, could be clearer
func lexIdent(x: Lexer): LexerReturn =
  if x.current.isLetter:
    var future: int
    for ch in x.stream:
      if ch.isLetter or ch.isNumber:
        future.inc
      else:
        if ch == ExportMarker:
          future.inc
        break
    (x.viewToken(tkIdent, future), future)
    # Token(kind: tkIdent, repr: x.futureSlice(future))
  else:
    (Token(kind: tkError), 0)


func lexKeyword(x: Lexer): LexerReturn =
  var future: int
  for ch in x.stream:
    if not ch.isLetter: break
    future.inc
  for keyword in ReservedWords:
    if x.view(future) == keyword.toOpenArray(keyword.low, keyword.high):
      return (Token(kind: tkKeyword), future)
  (Token(kind: tkError), 0)


func lexString(x: Lexer): LexerReturn =
  if x.current in StringMarkers:
    var future = 1
    for cur in x.stream(x.nextCursor):
      if cur == EndChar: break # no enclosing marker encountered
      future.inc
      if cur in StringMarkers:
        # return Token(kind: tkValue, repr: x.futureSlice(future))
        return (x.viewToken(tkValue, future), future)
  (Token(kind: tkError), 0)


func lexInt(x: Lexer): LexerReturn =
  if x.current.isNumber:
    var future: int
    for ch in x.stream:
      if ch.isNumber:
        future.inc
      else: break
    (x.viewToken(tkValue, future), future)
    # Token(kind: tkValue, repr: x.futureSlice(future))
  else:
    (Token(kind: tkError), 0)


# todo: maybe create unified way of definition for all such symbols?
## example:
# symbols:
#   tkDot: '.',
#   tkAssign: '=',
#   ...
func lexSymbol(x: Lexer): LexerReturn =
  let kind = case x.current:
    of '.': tkDot
    of '=': tkAssign
    of ':': tkColon
    of '[': tkListOpen
    of ']': tkListClose
    else:
      return (Token(kind: tkError), 0)
  (Token(kind: kind), 1)


const lexValue = lexRule(lexInt, lexString)
const lexModule* = lexRule(lexSymbol, lexIdent, lexKeyword, lexValue)

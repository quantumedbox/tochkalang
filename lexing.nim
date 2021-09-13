import lexer
export lexer


# todo: specify descriptive error text on erroneous lex return for future displaying

# todo: do we need to keep info about spaces? or guess it from context

# todo: make export maker its own token for ease of parsing

# todo: maybe it is possible to parse idents and keywords at the same func?
#       -- possible impl:
#          . at compile time max possible keyword len is found
#          . on parsing all literal chars eaten
#          . if len of eaten sequence is larger than possible keyword len - it's identifier
#          . then we can optimally traverse sequence for keyword match, possibly by introducing
#            some special data structure, on fail - treat it as identifier

# todo: it's possible for tkNewIndent to have no additional attached data as indentation
#       might be written in vacant 'head' or 'tail' fields which aren't used

# todo: should all tokens store their position in source? it might be helpful for constructing
#       some debugging information, ast trees then could infer to source range


func lexIdent(x: Lexer): LexRet =
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
  else:
    (Token(kind: tkError), 0)


func lexKeyword(x: Lexer): LexRet =
  var future: int
  for ch in x.stream:
    if not ch.isLetter: break
    future.inc
  for keyword in ReservedWords:
    if x.view(future) == keyword.toOpenArray(keyword.low, keyword.high):
      return (Token(kind: tkKeyword), future)
  (Token(kind: tkError), 0)


func lexString(x: Lexer): LexRet =
  if x.current == StringMarker:
    var future = 1
    for cur in x.stream(x.cursor + 1):
      if cur == EndChar: break # no enclosing marker encountered
      future.inc
      if cur == StringMarker:
        return (x.viewToken(tkString, future), future)
  (Token(kind: tkError), 0)


func lexInt(x: Lexer): LexRet =
  if x.current.isNumber:
    var future: int
    for ch in x.stream:
      if ch.isNumber:
        future.inc
      else: break
    (x.viewToken(tkInt, future), future)
  else:
    (Token(kind: tkError), 0)


# todo: maybe create unified way of definition for all such symbols?
## example:
# symbols:
#   tkDot: '.',
#   tkAssign: '=',
#   ...
func lexSymbol(x: Lexer): LexRet =
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

import std/strutils
export strutils.contains

type
  TokenKind* = enum
    tkError,                    # Signal for caller that parsing wasn't successful
    tkIdent,                    # Literal symbol
    tkValue,                    # Any value that is known without context
    tkKeyword,                  # Literal symbol protected from being an tkIdent
    tkDot,                      # '.'
    tkAssign,                   # '='
    tkColon,                    # Used for opening scopes and specifying return type
    tkListOpen, tkListClose,    # '[', ']'
    tkNewline,                  # {'\n', '\r'}
    tkNewIndent,                # Change of indentation level

  TokenValueKind* = enum
    tvNone,
    tvInt,

  Token* = object
    kind*: TokenKind
    repr*: string
    case valueKind: TokenValueKind
    of tvNone: discard
    of tvInt:
      vInt*: int
    # cursor*: int

  Lexer* = object
    source*: string
    cursor*: int
    lineno*: int
    indent*: int
    tokens*: seq[Token]
    lexfun*: proc(p: var Lexer)

  LexerError* = object of CatchableError


const
  EndChar* = '\0' # todo: not sure about char choise, maybe unicode has something for that
  IndentChar* = ' '
  IndentTemplate* = "  "
  SpecialCharTokens* = {tkDot, tkAssign, tkColon}


func initLexer*(src: string, fn: proc(p: var Lexer)): Lexer =
  result = Lexer(lexfun: fn, lineno: 1)
  shallowCopy result.source, src


func lexerStateInfo(p: Lexer): string =
  "cursor pos: " & $p.cursor & ", lineno: " & $p.lineno


proc tokenize*(p: var Lexer): bool =
  try:
    p.lexfun(p)
    result = true
  except LexerError as err:
    echo "Error while lexing: ", err.msg, '\n', p.lexerStateInfo


func atEnd*(p: Lexer): bool {.inline.} =
  p.cursor >= p.source.len - 1


func addToken*(p: var Lexer, kind: TokenKind) {.inline.} =
  p.tokens.add Token(kind: kind)


# func addToken*(p: var Lexer, kind: TokenKind, repr: string) {.inline.} =
#   p.tokens.add Token(kind: kind, valueKind: tvString, repr: repr)


func addToken*(p: var Lexer, tok: Token) {.inline.} =
  p.tokens.add tok


func nextCursor*(p: Lexer): int {.inline.} =
  p.cursor+1


func valid*(t: Token): bool {.inline.} =
  t.kind != tkError


func current*(p: Lexer): char {.inline.} =
  p.source[p.cursor]


func next*(p: Lexer): char {.inline.} =
  if not p.atEnd:
    p.source[p.nextCursor]
  else:
    EndChar


# todo: maybe just use sets in code ? tho there might be some complex
#       char checks to do
func isSpace*(c: char): bool {.inline.} =
  c in Whitespace


func isNewline*(c: char): bool {.inline.} =
  c in Newlines


func isLetter*(c: char): bool {.inline.} =
  c in {'a'..'z', 'A'..'Z'}


func isNumber*(c: char): bool {.inline.} =
  c in Digits


func futureSlice*(p: Lexer, i: int): string {.inline.} =
  p.source[p.cursor..<(p.cursor + i)]


proc `$`*(p: Lexer): string =
  for i, token in p.tokens:
    result = result & $token
    if i != p.tokens.len - 1:
      result = result & '\n'


proc `$`*(t: Token): string =
  $t.kind & " : " & t.repr


# todo: yeah, i love repeating code
iterator stream*(p: Lexer): char {.inline.} =
  var pos = p.cursor
  while pos < p.source.len:
    yield p.source[pos]
    pos.inc


iterator pairstream*(p: Lexer): tuple[idx: int, ch: char] {.inline.} =
  var pos = p.cursor
  var idx: int
  while pos < p.source.len:
    yield (idx, p.source[pos])
    pos.inc
    idx.inc


iterator futurestream*(p: Lexer): tuple[cur: char, next: char] {.inline.} =
  var pos = p.cursor
  while pos < p.source.len - 1:
    yield (p.source[pos], p.source[pos + 1])
    pos.inc
  yield (p.source[pos], EndChar)


iterator stream*(p: Lexer, start: int): char {.inline.} =
  var pos = start
  while pos < p.source.len:
    yield p.source[pos]
    pos.inc


func eatToken*(p: var Lexer, tok: Token) {.inline.} =
  p.addToken tok
  if tok.kind in SpecialCharTokens:
    p.cursor.inc
  else:
    p.cursor += tok.repr.len


proc eatIndent*(p: var Lexer) =
  var future: int
  for cur in p.stream:
    if cur != IndentChar: break
    future.inc
  if future %% 2 != 0:
    raise newException(LexerError, "invalid indentation")
  let level = p.futureSlice(future).count(IndentTemplate)
  if level != p.indent:
    p.addToken Token(kind: tkNewIndent, valueKind: tvInt, vInt: level)
    p.indent = level


proc eatSpace*(p: var Lexer) =
  var windowsSkip: bool
  for cur, next in p.futurestream:
    if windowsSkip:
      windowsSkip = false
    elif cur == '\r' and next == '\n': # windows newline
      p.addToken tkNewline
      p.lineno.inc
      p.cursor += 2
      p.eatIndent
      windowsSkip = true
    elif cur == '\n': # unix newline
      p.addToken tkNewline
      p.lineno.inc
      p.cursor.inc
      p.eatIndent
    elif cur.isSpace:
      p.cursor.inc
    else: break

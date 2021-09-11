import std/[strutils, macros]
import language

type
  TokenValueKind* = enum
    tvNone,
    tvInt,

  Token* = object
    kind*: TokenKind
    head*: int # Beginning within source
    tail*: int # End within source
    case valueKind: TokenValueKind
    of tvNone: discard
    of tvInt:
      vInt*: int

  Lexer* = object
    source*: string # todo: maybe it should be ref? as it will be passed around
    cursor*: int
    lineno*: int
    indent*: int
    tokens*: seq[Token]
    lexfun: proc(p: var Lexer)

  LexerError* = object of CatchableError


func initLexer*(src: string, fn: proc(p: var Lexer)): Lexer =
  result = Lexer(lexfun: fn, lineno: 1)
  shallowCopy result.source, src


func prepareSource*(p: var Lexer)
func stateInfo(p: Lexer): string
proc tokenize*(p: var Lexer): bool =
  try:
    p.prepareSource
    p.lexfun(p)
    result = true
  except LexerError as err:
    echo "Error while lexing: ", err.msg, '\n', p.stateInfo


func atEnd*(p: Lexer): bool {.inline.} =
  p.cursor >= p.source.len - 1


func addToken*(p: var Lexer, kind: TokenKind) {.inline.} =
  p.tokens.add Token(kind: kind)


func addToken*(p: var Lexer, tok: Token) {.inline.} =
  p.tokens.add tok


func nextCursor*(p: Lexer): int {.inline.} =
  p.cursor+1


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


func viewToken*(p: Lexer, k: TokenKind, future: int): Token {.inline.} =
  Token(kind: k, head: p.cursor, tail: p.cursor + future)


macro view*(p: Lexer, future: int) =
  # Shortcut for:
  # toOpenArray(p.source, p.cursor, p.cursor + future)
  newCall(
    ident("toOpenArray"),
    newDotExpr(p, ident("source")),
    newDotExpr(p, ident("cursor")),
    infix(
      newDotExpr(p, ident("cursor")),
      "+",
      future
    )
  )


# func futureSlice*(p: Lexer, i: int): string {.inline.} =
#   p.source[p.cursor..<(p.cursor + i)]


func valid*(t: Token): bool {.inline.} =
  t.kind != tkError


# todo: yeah, i love repeating code
iterator stream*(p: Lexer): char {.inline.} =
  var pos = p.cursor
  while pos < p.source.len:
    yield p.source[pos]
    pos.inc


iterator stream*(p: Lexer, start: int): char {.inline.} =
  var pos = start
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


iterator backstream*(p: Lexer): char {.inline.} =
  var pos = p.cursor
  while pos >= 0:
    yield p.source[pos]
    pos.dec


func eatToken*(p: var Lexer, tok: Token) {.inline.} =
  p.addToken tok
  if tok.kind in SpecialCharTokens:
    p.cursor.inc
  else:
    p.cursor += tok.tail - tok.head
    # p.cursor += tok.repr.len


func eatIndent*(p: var Lexer) =
  var future: int
  for cur in p.stream:
    if cur != IndentChar: break
    future.inc
  if future %% 2 != 0:
    raise newException(LexerError, "invalid indentation")
  let level = future div 2
  if level != p.indent:
    p.addToken Token(kind: tkNewIndent, valueKind: tvInt, vInt: level)
    p.indent = level


func eatSpace*(p: var Lexer) =
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


# todo: doesn't work in many cases, needs some special handling
func prepareSource*(p: var Lexer) =
  p.eatSpace
  if p.indent != 0:
    raise newException(LexerError, "invalid indentation")


func linepos*(p: Lexer): int =
  # Returns position relative to previous newline in the stream by walking back
  for ch in p.backstream:
    if ch == '\n': break
    result.inc


func `$`*(t: Token): string =
  $t.kind


func `$`*(p: Lexer): string =
  for i, token in p.tokens:
    result.add $token
    if token.tail - token.head != 0:
      result.add " : "
      result.add p.source[token.head..<token.tail] # todo: how to print it raw?
    if i != p.tokens.len - 1:
      result.add '\n'


func stateInfo(p: Lexer): string =
  result = "pos: "
  result.add $p.linepos
  result.add ", lineno: "
  result.add $p.lineno

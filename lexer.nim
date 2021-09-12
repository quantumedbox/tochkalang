import std/[strutils, macros]

type
  TokenKind* = enum
    tkNone,
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
    # lexfun: proc(x: var Lexer)

  LexerReturn* = tuple[tok: Token, progress: int]
  LexerFunc* = proc(x: Lexer): LexerReturn {.nimcall, noSideEffect.}
  LexerError* = object of CatchableError


const
  EndChar* = '\0' # todo: not sure about char choise, maybe unicode has something for that
  IndentChar* = ' '
  SpecialCharTokens* = {tkDot, tkAssign, tkColon, tkListOpen, tkListClose}

  ReservedWords* = ["mut", "type", "cond"] # should only consist of isLetter chars
  ExportMarker* = '*'
  StringMarkers* = {'"'}


func lexRuleUnify*(x: LexerFunc): seq[LexerFunc] = result.add x
func lexRuleUnify*[ix: static int](arr: array[ix, LexerFunc]): seq[LexerFunc] =
  for x in arr: result.add x
template lexRule*(feed: varargs[seq[LexerFunc], lexRuleUnify]): auto =
  const procs = static:
    var inline: seq[LexerFunc]
    for x in feed:
      for y in x:
        if y notin inline:
          inline.add y
    inline
  const result = static:
    var inplace: array[procs.len, LexerFunc]
    for i, x in procs:
      inplace[i] = x
    inplace
  result


func atEnd*(x: Lexer): bool {.inline.} =
  x.cursor >= x.source.len - 1


func addToken*(x: var Lexer, kind: TokenKind) {.inline.} =
  x.tokens.add Token(kind: kind)


func addToken*(x: var Lexer, tok: Token) {.inline.} =
  x.tokens.add tok


func nextCursor*(x: Lexer): int {.inline.} =
  x.cursor + 1


func current*(x: Lexer): char {.inline.} =
  x.source[x.cursor]


func next*(x: Lexer): char {.inline.} =
  if not x.atEnd:
    x.source[x.nextCursor]
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


func viewToken*(x: Lexer, k: TokenKind, future: int): Token {.inline.} =
  Token(kind: k, head: x.cursor, tail: x.cursor + future)


# func futureSlice*(x: Lexer, i: int): string {.inline.} =
#   x.source[x.cursor..<(x.cursor + i)]


func valid*(t: Token): bool {.inline.} =
  t.kind != tkError


macro view*(x: Lexer, future: int): untyped =
  ## Shortcut for:
  ## toOpenArray(x.source, x.cursor, x.cursor + future)
  newCall(
    ident("toOpenArray"),
    newDotExpr(x, ident("source")),
    newDotExpr(x, ident("cursor")),
    infix(
      newDotExpr(x, ident("cursor")),
      "+",
      future
    )
  )


# todo: yeah, i love repeating code
iterator stream*(x: Lexer): char {.inline, noSideEffect.} =
  var pos = x.cursor
  while pos < x.source.len:
    yield x.source[pos]
    pos.inc


iterator stream*(x: Lexer, start: int): char {.inline, noSideEffect.} =
  var pos = start
  while pos < x.source.len:
    yield x.source[pos]
    pos.inc


iterator pairstream*(x: Lexer): tuple[idx: int, ch: char] {.inline, noSideEffect.} =
  var pos = x.cursor
  var idx: int
  while pos < x.source.len:
    yield (idx, x.source[pos])
    pos.inc
    idx.inc


iterator futurestream*(x: Lexer): tuple[cur: char, next: char] {.inline, noSideEffect.} =
  var pos = x.cursor
  while pos < x.source.len - 1:
    yield (x.source[pos], x.source[pos + 1])
    pos.inc
  yield (x.source[pos], EndChar)


iterator backstream*(x: Lexer): char {.inline, noSideEffect.} =
  var pos = x.cursor
  while pos >= 0:
    yield x.source[pos]
    pos.dec


func eatIndent*(x: var Lexer) =
  var future: int
  for cur in x.stream:
    if cur != IndentChar: break
    future.inc
  if future %% 2 != 0:
    raise newException(LexerError, "invalid indentation")
  let level = future div 2
  if level != x.indent:
    x.addToken Token(kind: tkNewIndent, valueKind: tvInt, vInt: level)
    x.indent = level


func eatSpace*(x: var Lexer) =
  var windowsSkip: bool
  for cur, next in x.futurestream:
    if windowsSkip:
      windowsSkip = false
    elif cur == '\r' and next == '\n': # windows newline
      x.addToken tkNewline
      x.lineno.inc
      x.cursor += 2
      x.eatIndent
      windowsSkip = true
    elif cur == '\n': # unix newline
      x.addToken tkNewline
      x.lineno.inc
      x.cursor.inc
      x.eatIndent
    elif cur.isSpace:
      x.cursor.inc
    else: break


# todo: doesn't work in many cases, needs some special handling
func prepareSource*(x: var Lexer) =
  x.eatSpace
  if x.indent != 0:
    raise newException(LexerError, "invalid indentation")


# todo: not sure about that break jumping and checking for exceptions
func stateInfo(x: Lexer): string
proc tokenize*(src: string, rules: openArray[LexerFunc]): Lexer =
  var x = Lexer(lineno: 1)
  shallowCopy x.source, src
  try:
    var ret: LexerReturn
    x.prepareSource
    while not x.atEnd:
      for rule in rules:
        ret = x.rule
        if ret.tok.valid:
          x.tokens.add ret.tok
          x.cursor += ret.progress
          x.eatSpace
          break
      if not ret.tok.valid:
        raise newException(LexerError, "unknown token")
  except LexerError as err:
    echo "Error while lexing: ", err.msg, '\n', x.stateInfo
  result = x


func linepos*(x: Lexer): int =
  ## Returns position relative to previous newline in the stream by walking back
  for ch in x.backstream:
    if ch == '\n': break
    result.inc


func `$`*(t: Token): string =
  ## Because repr information is in source and tokens themselves
  ## have no idea about their context - you can't print anything
  $t.kind


func `$`*(x: Lexer): string =
  for i, token in x.tokens:
    result.add $token
    if token.tail - token.head != 0:
      result.add " : "
      result.add x.source[token.head..<token.tail] # todo: how to print it raw?
    if i != x.tokens.len - 1:
      result.add '\n'


func stateInfo(x: Lexer): string =
  result = "pos: "
  result.add $x.linepos
  result.add ", lineno: "
  result.add $x.lineno

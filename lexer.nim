import std/[strutils, macros]

# todo: token to string repr utility

# todo: introduction of EOF token can help in certain cases
#       this way checking for EOF can be done via the same methods as other tokens
#       and not cursor/bound checks

# todo: storing everything in a single array rises a problem:
#       big sequence could take noticeable time for reallocation
#       we could introduce growing parallel array structure

type
  TokenKind* = enum
    tkNone,                     # Default for not triggering error by clear initialization
    tkError,                    # Signal for caller that parsing wasn't successful
    tkEndOfFile,
    tkNewline,                  # {'\n', '\r'}
    tkNewIndent,                # Change of indentation level
    tkIdent,                    # Literal symbol
    tkInt,
    tkString,
    tkDot,                      # '.'
    tkAssign,                   # '='
    tkColon,                    # ':' Used for opening scopes and specifying return type
    tkListOpen, tkListClose,    # '[', ']'
    tkIf, tkElif, tkElse,
    tkCond, tkCase, tkOf,

  TokenValueKind* = enum
    tvNone,
    tvInt,
    tvUint,

  Token* = object
    kind*: TokenKind
    head*: int # Beginning within source
    tail*: int # End within source
    case valueKind: TokenValueKind
    of tvNone: discard
    of tvInt:
      vInt*: int
    of tvUint:
      vUint*: uint

  Lexer* = object
    source*: string # todo: maybe it should be ref? as it will be passed around
    cursor*: int
    lineno*: uint
    indent*: uint
    tokens*: seq[Token]

    # Extra hints
    initialIndent*: uint

  LexRet* = tuple[tok: Token, progress: int]
  LexError* = object of CatchableError
  LexDef* = proc(x: Lexer): LexRet {.nimcall, noSideEffect, raises: [LexError], gcsafe.}

const
  EndChar* = '\0' # todo: not sure about char choise, maybe unicode has something for that
  IndentChar* = ' '
  ExportMarker* = '*'
  StringMarker* = '"'


func lexRuleUnify*(x: LexDef): seq[LexDef] = result.add x
func lexRuleUnify*[ix: static int](arr: array[ix, LexDef]): seq[LexDef] = result.add arr
template lexRule*(feed: varargs[seq[LexDef], lexRuleUnify]): auto =
  const procs = static:
    var inline: seq[LexDef]
    for x in feed:
      for y in x:
        if y notin inline:
          inline.add y
    inline
  const result = static:
    var emplace: array[procs.len, LexDef]
    for i, x in procs:
      emplace[i] = x
    emplace
  result


# todo: compile-time tree of nodes for fast lookup of keywords char by char
#       we can also optimize by using arrays and indexes that correspond with characters
#       if keywords are only in ASCII - 256 possible elems per variant shouldn't add too much
#       non-valid variants should be 0 mem then
type
  KeywordTreeNode = object
    ch: char
    points: ptr KeywordTreeNode

template push(a: KeywordTreeNode): untyped =
  discard


template view*(x: Lexer, future: int): auto =
  toOpenArray(x.source, x.cursor, x.cursor + future - 1)


func eatIndent(x: var Lexer, start: int): int
func initLexer(source: sink string): Lexer =
  result.source = source
  result.lineno = 1
  result.cursor = result.eatIndent 0
  result.tokens.reset
  result.tokens.add Token(kind: tkNone)
  result.initialIndent = result.indent


{.push inline, noSideEffect.}

func `[]`*(x: Lexer, i: Natural): char =
  if i in x.source.low..x.source.high:
    x.source[i]
  else: EndChar

func atEnd*(x: Lexer): bool =
  x.cursor >= x.source.len

func addToken*(x: var Lexer, kind: TokenKind) =
  x.tokens.add Token(kind: kind)

func addToken*(x: var Lexer, tok: Token) =
  x.tokens.add tok

func current*(x: Lexer): char =
  x.source[x.cursor]

func next*(x: Lexer): char =
  if not x.atEnd:
    x.source[x.cursor + 1]
  else: EndChar

# todo: maybe just use sets in code ? tho there might be some complex
#       char checks to do
func isSpace*(c: char): bool =
  c in Whitespace

func isNewline*(c: char): bool =
  c in Newlines

func isSpaceNotNewline*(c: char): bool =
  c in Whitespace - Newlines

func isLetter*(c: char): bool =
  c in {'a'..'z', 'A'..'Z'}

func isNumber*(c: char): bool =
  c in Digits


func viewToken*(x: Lexer, k: TokenKind, future: int): Token =
  Token(kind: k, head: x.cursor, tail: x.cursor + future)


func valid*(t: Token): bool =
  t.kind != tkError


# todo: yeah, i love repeating code
iterator stream*(x: Lexer): char =
  var pos = x.cursor
  while pos < x.source.len:
    yield x.source[pos]
    pos.inc


iterator stream*(x: Lexer, start: int): char =
  var pos = start
  while pos < x.source.len:
    yield x.source[pos]
    pos.inc


iterator pairstream*(x: Lexer): tuple[idx: int, ch: char] =
  var pos = x.cursor
  var idx: int
  while pos < x.source.len:
    yield (idx, x.source[pos])
    pos.inc
    idx.inc


iterator nextstream*(x: Lexer): tuple[cur: char, next: char] =
  var pos = x.cursor
  while pos < x.source.len - 1:
    yield (x.source[pos], x.source[pos + 1])
    pos.inc
  if pos < x.source.len:
    yield (x.source[pos], EndChar)


iterator backstream*(x: Lexer): char =
  var pos = x.cursor
  while pos >= 0:
    yield x.source[pos]
    pos.dec

{.pop.}

# todo: ? is checking for "\r\n" necessary? we can simplify it if it's just either '\r' or '\n' for newline
func eatIndent(x: var Lexer, start: int): int =
  ## Used for skipping empty lines and detecting change of indentation
  var indent: int
  var windowsSkip: bool
  var cursor = start
  while x[cursor] != EndChar:
    let cur = x[cursor]
    let next = x[cursor + 1]
    if windowsSkip: windowsSkip = false
    elif cur == IndentChar:
      indent.inc
      cursor.inc
    elif cur == '\r' and next == '\n': # windows newline
      cursor += 2
      windowsSkip = true
      indent.reset
    elif cur == '\n': # unix newline
      cursor.inc
      indent.reset
    else:
      if indent.uint mod 2 != 0:
        raise newException(LexError, "invalid indentation of " & $indent & " amount of spaces")
      let level = indent.uint div 2
      if level != x.indent:
        x.addToken Token(kind: tkNewIndent, valueKind: tvUint, vUint: level)
        x.indent = level
      else:
        x.addToken tkNewline
      break
  cursor

func eatSpace*(x: var Lexer) =
  var cursor = x.cursor
  while x[cursor] != EndChar:
    let cur = x[cursor]
    if cur.isSpaceNotNewline:
      cursor.inc
    elif cur == '\r' and x[cursor + 1] == '\n': # windows newline
      x.lineno.inc
      cursor = x.eatIndent(cursor + 2)
    elif cur == '\n': # unix newline
      x.lineno.inc
      cursor = x.eatIndent(cursor + 1)
    else: break
  x.cursor = cursor


# todo: not sure about that break jumping and checking for exceptions
func stateInfo(x: Lexer): string
proc tokenize*(src: sink string, rules: openArray[LexDef]): Lexer =
  var x = initLexer(src)
  try:
    var ret: LexRet
    while not x.atEnd:
      for rule in rules:
        ret = x.rule
        if ret.tok.valid:
          x.tokens.add ret.tok
          x.cursor += ret.progress
          x.eatSpace
          break
      if not ret.tok.valid:
        raise newException(LexError, "unknown token " & x.current)
  except LexError as err:
    echo "Error while lexing: ", err.msg, '\n', x.stateInfo
  x.tokens.add Token(kind: tkEndOfFile)
  result = x


func linepos*(x: Lexer): int =
  ## Returns position relative to previous newline in the stream by walking back
  for ch in x.backstream:
    if ch == '\n': break
    result.inc


func `$`*(x: Lexer): string =
  for i, token in x.tokens:
    result.add $i
    result.add ": " # todo: line it up to certain common indentation
    result.add $token.kind
    if token.tail - token.head != 0:
      result.add " : "
      result.add x.source[token.head..<token.tail] # todo: print it raw?
    case token.valueKind
    of tvInt:
      result.add " : "
      result.add $token.vInt
    of tvUint:
      result.add " : "
      result.add $token.vUint
      result.add "u"
    else: discard
    if i != x.tokens.high:
      result.add '\n'


func stateInfo(x: Lexer): string =
  result = "pos: "
  result.add $x.linepos
  result.add ", lineno: "
  result.add $x.lineno

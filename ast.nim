import strutils
import lexer


# todo: show more detailed information about errors and AstState


## GrammarDef requirements:
#   each non error def call should leave cursor on next positions after consumed part
#   restoring state on error - responsibility of def itself


type
  AstKind* = enum
    nkNone,       # Default for not triggering error by clear initialization
    nkError,
    nkPair,       # Special kind for implementing lists within buffer
    nkIdent,
    nkKeyword,
    nkInt,
    nkFloat,
    nkString,
    # nkExpr,
    nkStmt,
    nkStmtList,
    nkList,
    nkProc,
    nkAssign,
    nkDef,

  AstNode* = object
    kind*: AstKind
    head*: int      # Beginning in source, as offset
    tail*: int      # Ending in source, as offset
    # Each side holds an offset in shared node sequence
    # By limiting each node to only hold certain amount of other nodes
    # we don't need to allocate any extra heap data and illuminate indirection
    left*: int
    right*: int

  AstState* = object
    # Main idea behind this implementation is to achieve cache locality
    # by utilizing single buffer for all ast nodes and make them reference
    # each other by buffer offset instead of pointers to heap allocated space
    source*: string
    nodes*: seq[AstNode]
    tokens*: seq[Token]
    cursor*: int          # Position in tokens
    entries*: seq[int]    # Indexes of 'nodes' that are top-most
    indent*: int

  GrammarRet* = tuple[node: AstNode, future: int]
  GrammarError* = object of CatchableError
  GrammarDef* = proc(s: var AstState, start: int): GrammarRet {.nimcall, noSideEffect, raises: [GrammarError], gcsafe.}
  # GrammarDef* = proc(s: var AstState, start: int): GrammarRet {.nimcall, raises: [GrammarError].}

  # GhostState* = tuple[cursor: int, nodes: int] # Used for restoring state after failure


const EmptyIndex: int = 0


func `[]`*(s: AstState, i: Natural): Token {.inline.} =
  if i in s.tokens.low..s.tokens.high:
    s.tokens[i]
  else:
    Token(kind: tkError)


func isEnd*(s: AstState, i: Natural): bool {.inline.} =
  i >= s.tokens.len


# func atEnd*(s: AstState): bool {.inline.} =
#   s.cursor >= s.tokens.len


# func current*(s: AstState): Token {.inline.} =
#   s.tokens[s.cursor]


# func next*(s: AstState): Token {.inline.} =
#   if not s.atEnd:
#     s.tokens[s.cursor + 1]
#   else:
#     Token(kind: tkNone)


# func oracle*(s: AstState, future: Natural): Token {.inline.} =
#   s.tokens[s.cursor + future]


# func advance*(s: var AstState, i: Natural = 1) {.inline.} =
#   s.cursor += i


func emplace*(s: var AstState, n: AstNode): int {.inline.} =
  s.nodes.add n
  s.nodes.high


# func catchGhost*(s: AstState): GhostState {.inline.} =
#   (s.cursor, s.nodes.len)


# func letgo*(s: var AstState, g: GhostState) {.inline.} =
#   s.cursor = g.cursor
#   s.nodes.setLen(g.nodes)


func letgo*(s: var AstState, i: Natural) {.inline.} =
  s.nodes.setLen(i)


func valid*(n: AstNode): bool {.inline.} =
  n.kind != nkError and n.kind != nkNone


proc parse*(x: Lexer, rules: openArray[GrammarDef]): AstState =
  var s: AstState
  shallowCopy s.source, x.source
  shallowCopy s.tokens, x.tokens
  s.nodes.add AstNode(kind: nkNone) # dummy node that is reserved for 'null'
  try:
    var ret: GrammarRet
    var cursor: int
    while not s.isEnd(cursor):
      for rule in rules:
        ret = s.rule(cursor)
        if ret.node.valid:
          if ret.future <= cursor:
            raise newException(GrammarError, "infinite recursion as cursor wasn't progressed from rule")
          s.entries.add(s.emplace(ret.node))
          cursor = ret.future
          break
      if not ret.node.valid:
        raise newException(GrammarError, "unknown grammar at pos " & $s.tokens[cursor].head)
  except GrammarError as err:
    echo "Grammatical error: ", err.msg
  result = s


iterator stream*(s: var AstState, i: Natural): Token {.inline, noSideEffect.} =
  var pos = i
  while pos < s.tokens.len:
    yield s.tokens[pos]
    pos.inc


# iterator stream*(s: var AstState): Token {.inline, noSideEffect.} =
#   # Warn! This advances cursor state
#   while s.cursor < s.tokens.len:
#     yield s.tokens[s.cursor]
#     s.cursor.inc


func `$`*(s: AstState): string =
  func recurToStr(s: AstState, n: AstNode, indent: int = 0): string =
    for i in 0..<indent:
      result.add "->"
    result.add $n.kind
    if n.head - n.tail != 0:
      result.add " : "
      result.add s.source[n.head..<n.tail]
    result.add '\n'
    for side in [n.left, n.right]:
      if side != EmptyIndex:
        result.add s.recurToStr(s.nodes[side], indent + 1)

  for e, entry in s.entries:
    result.add s.recurToStr(s.nodes[entry])
    if e != s.entries.high:
      result.add '\n'

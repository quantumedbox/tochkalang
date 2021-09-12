import strutils
import lexer

# todo: notable side effect of such implementation is the fact that we can't traverse
#       resulting token list without information about offset of token structure
#       as all nest levels are inlined in one buffer
#       - possible solution for this is storing buffer offsets of all top-level tokens
#       - alternatively we can traverse tree from top to bottom and calculate the size
#           but it isn't really desirable

# todo: show more detailed information about errors and AstState

# todo: ability to store 'none' value in left/right fields of AstNode
#       0 should reference nodes buffer, something like -1 then is default
#       alternatively we can offset indexes by 1 and then first elem will be 1
#       but it will fuck up my and everyone else's brain, so, no
#       -- more stupid but kinda genius solution: just add dummy AstNode at the beginning
#          of nodes buffer and don't have it registered in entries - then it will not be even
#          accessible through current API


## GrammarDef requirements:
#   each non error def should leave cursor on next positions after consumed part
#   restoring state on error - task of def itself


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
    nkDefinition,

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
    entries*: seq[int]    # Indexes of 'nodes' seq that are top-most

  GrammarRet* = tuple[node: AstNode, future: int]
  GrammarDef* = proc(s: var AstState): GrammarRet {.nimcall.}
  # GrammarDef* = proc(s: var AstState): GrammarRet {.nimcall, noSideEffect.}
  GrammarError* = object of CatchableError

  GhostState* = tuple[cursor: int, nodes: int] # Used for restoring state after failure


const EmptyIndex: int = 0


# func initAstNode*(kind: AstKind = nkNone, head, tail: int = 0, left, right: int = EmptyIndex): AstNode =
#   AstNode(kind: kind, head: head, tail: tail, left: left, right: right)


func atEnd*(s: AstState): bool {.inline.} =
  s.cursor >= s.tokens.len - 1


func current*(s: AstState): Token {.inline.} =
  s.tokens[s.cursor]


func next*(s: AstState): Token {.inline.} =
  if not s.atEnd:
    s.tokens[s.cursor + 1]
  else:
    Token(kind: tkNone)


func oracle*(s: AstState, future: int): Token {.inline.} =
  s.tokens[s.cursor + future]


func advance*(s: var AstState, i: int = 1) {.inline.} =
  s.cursor += i


func emplace*(s: var AstState, n: AstNode): int {.inline.} =
  s.nodes.add n
  s.nodes.high


func catchGhost*(s: AstState): GhostState {.inline.} =
  (s.cursor, s.nodes.len)


func letgo*(s: var AstState, g: GhostState) {.inline.} =
  s.cursor = g.cursor
  s.nodes.setLen(g.nodes)


func valid*(n: AstNode): bool {.inline.} =
  n.kind != nkError


proc parse*(x: Lexer, rules: openArray[GrammarDef]): AstState =
  var s: AstState
  shallowCopy s.source, x.source
  shallowCopy s.tokens, x.tokens
  s.nodes.add AstNode(kind: nkNone) # dummy node that is reserved for 'null' of node left/right branches
  try:
    var ret: GrammarRet
    while not s.atEnd:
      for rule in rules:
        ret = s.rule
        if ret.node.valid:
          s.entries.add s.emplace ret.node
          break
      if not ret.node.valid:
        raise newException(GrammarError, "unknown grammar")
  except GrammarError as err:
    echo "Grammatical error: ", err.msg
  result = s


iterator stream*(s: var AstState): Token {.inline, noSideEffect.} =
  # Warn! This advances cursor state
  while s.cursor < s.tokens.len:
    yield s.tokens[s.cursor]
    s.cursor.inc


func `$`*(s: AstState): string =
  func recurToStr(s: AstState, n: AstNode, indent: int = 0): string =
    for i in 0..<indent:
      result.add "-->"
    result.add $n.kind
    result.add '\n'
    if n.left != 0:
      result.add s.recurToStr(s.nodes[n.left], indent + 1)
    if n.right != 0:
      result.add s.recurToStr(s.nodes[n.right], indent + 1)

  for e, entry in s.entries:
    result.add s.recurToStr(s.nodes[entry])
    if e != s.entries.high:
      result.add '\n'

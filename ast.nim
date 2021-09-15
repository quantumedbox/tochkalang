import strutils
import lexer


# todo: show more detailed information about errors and AstState

# todo: nodes can have union of left/right and head/tail values
#       as head/tail is usually required only for values and don't have hierarchy
#       and structural nodes don't have representation in source (at least which matters for codegen)
#       only hierarchy relations are important for them

# todo: make that buffer capacity should grow in optimal/configurable chunks and not single elements

# todo: possible optimization: store addresses of nodes within node branches, this way on sequence indexing
#       you wouldn't need calculating offset from base and bound check
#       safety-wise it should be okay as nodes do not permanently allocate unless they're successful at which point they're static


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
    nkBody,
    nkList,
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
    tokens*: seq[Token]
    nodes*: seq[AstNode]
    # cursor*: int        # Position in tokens
    entries*: seq[int]    # Indexes of 'nodes' that are top-most
    indent*: uint

  GrammarRet* = tuple[node: AstNode, future: int]
  GrammarError* = object of CatchableError
  GrammarDef* = proc(s: var AstState, start: int): GrammarRet {.nimcall, noSideEffect, raises: [GrammarError], gcsafe.}

const
  EmptyIndex: int = 0


func `[]`*(s: AstState, i: Natural): Token {.inline.} =
  if i in s.tokens.low..s.tokens.high:
    s.tokens[i]
  else:
    Token(kind: tkError)


func isEnd*(s: AstState, i: Natural): bool {.inline.} =
  i >= s.tokens.len


func emplace*(s: var AstState, n: AstNode): int {.inline.} =
  s.nodes.add n
  s.nodes.high


func letgo*(s: var AstState, i: Natural) {.inline.} =
  s.nodes.setLen(i)


func valid*(n: AstNode): bool {.inline.} =
  n.kind != nkError and n.kind != nkNone


proc parse*(x: Lexer, rules: openArray[GrammarDef]): AstState =
  var s: AstState
  shallowCopy s.source, x.source
  shallowCopy s.tokens, x.tokens
  s.indent = x.initialIndent
  s.nodes.add AstNode(kind: nkNone) # dummy node that is reserved for 'null'
  try:
    var ret: GrammarRet
    var cursor: int
    while not s.isEnd(cursor):
      for rule in rules:
        ret = s.rule(cursor)
        if ret.node.valid:
          if ret.future <= cursor:
            raise newException(GrammarError, "infinite recursion prevented as future cursor is equal or less of present")
          s.entries.add(s.emplace(ret.node))
          cursor = ret.future
          break
      if not ret.node.valid:
        raise newException(GrammarError, "unknown grammar at pos " & $s.tokens[cursor].head) #todo
  except GrammarError as err:
    echo "Grammatical error: ", err.msg
  result = s


iterator stream*(s: var AstState, i: Natural): Token {.inline, noSideEffect.} =
  var pos = i
  while pos < s.tokens.len:
    yield s.tokens[pos]
    pos.inc


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

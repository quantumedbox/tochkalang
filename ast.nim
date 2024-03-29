import macros
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

# todo: storing everything in a single array rises a problem:
#       big sequence could take noticeable time for reallocation

# todo: left and right sides should have some defined rules on how sides are chosen for certain cases

# todo: don't use nkNone for error, just nkError is enough

type
  AstKind* = enum
    nkNone,     # Might be used for indicating no value in positional dependent lists
    nkError,
    nkPair,     # Special kind for implementing lists within buffer
    nkIdent,
    nkKeyword,
    nkInt,
    nkFloat,
    nkString,
    nkBody,
    nkList,
    nkAssign,
    nkDef,
    nkIfExpr, nkElifBranch, nkElseBranch
    nkNamedTuple, nkNamedTuplePair
    nkCall,
    nkInit,

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
    entries*: seq[int]    # Indexes of 'nodes' that are top-most # todo: can actually be just first actual node
    indent*: uint

  GrammarRet* = tuple[node: AstNode, future: int]
  GrammarError* = object of CatchableError
  GrammarDef* = proc(s: var AstState, start: int): GrammarRet {.nimcall, noSideEffect, raises: [GrammarError].}

const
  EmptyIndex*: int = 0
  ScopedNodes* = {nkBody, nkIfExpr}
  SequenceNodes* = {nkPair, nkBody, nkIfExpr}


template raiseGrammarError*(msg: string): untyped =
  raise newException(GrammarError, msg)


{.push inline.}

func initAst*(kind: AstKind, left, right, head = 0, tail: int = 0): AstNode =
  AstNode(kind: kind, left: left, right: right, head: head, tail: tail)

func `[]`*(s: AstState, i: Natural): lent Token =
  if i in s.tokens.low..s.tokens.high:
    result = s.tokens[i]
  else: # todo: just make it panic? it might be better than such access
    result = s.tokens[0] # 0 index always has tkNone

# func lastNode*(s: AstState): AstNode =
#   s.nodes[s.nodes.high]

func isEnd*(s: AstState, i: Natural): bool =
  s.tokens[i].kind == tkEndOfFile

func emplace*(s: var AstState, n: AstNode): int =
  s.nodes.add n
  s.nodes.high

# func letgo*(s: var AstState, i: Natural) =
#   s.nodes.setLen i

func valid*(n: AstNode): bool =
  n.kind != nkError and n.kind != nkNone

{.pop.}


proc parse*(x: Lexer, rules: openArray[GrammarDef]): AstState =
  var s: AstState
  shallowCopy s.source, x.source
  shallowCopy s.tokens, x.tokens
  s.indent = x.initialIndent
  s.nodes.add AstNode(kind: nkNone) # dummy node that is reserved for 'null'
  try:
    var ret: GrammarRet
    var cursor: int = 1
    while not s.isEnd(cursor):
      for rule in rules:
        ret = s.rule(cursor)
        if ret.node.valid:
          if ret.future <= cursor:
            raise newException(GrammarError, "infinite recursion at " & $cursor)
          s.entries.add(s.emplace(ret.node))
          cursor = ret.future
          break
      if not ret.node.valid:
        raise newException(GrammarError, "unknown grammar at pos " & $cursor) # todo: more helpful info
  except GrammarError as err:
    echo "Grammatical error: ", err.msg
  result = s


iterator stream*(s: var AstState, i: Natural): Token {.inline, noSideEffect.} =
  var pos = i
  while pos < s.tokens.len:
    yield s.tokens[pos]
    pos.inc


func nodeToString*(s: AstState, n: AstNode, indent: int = 0): string =
  for i in 0..<indent:
    result.add "->"
  result.add $n.kind
  if n.head != 0 or n.tail != 0:
    result.add " : "
    result.add s.source[n.head..n.tail]
  result.add '\n'
  for side in [n.left, n.right]:
    if side != EmptyIndex:
      result.add s.nodeToString(s.nodes[side], indent + 1)

func `$`*(s: AstState): string =
  for e, entry in s.entries:
    result.add s.nodeToString(s.nodes[entry])
    if e != s.entries.high:
      result.add '\n'


## Rule utilities

macro ruleAny*(s: var AstState, start: int, defs: openArray[GrammarDef]): untyped =
  ## Return result of first matching rule
  let match = ident"match" # todo: use gensym?
  var flow = newStmtList(quote do:
    var `match`: GrammarRet
  )
  let topdef = defs[^1]
  var top = quote do: `s`.`topdef`(`start`)
  for i in 2..defs.len:
    let leveldef = defs[^i]
    var level = newStmtList(newAssignment(match, quote do: `s`.`leveldef`(`start`)))
    let ifelse = newIfStmt((newCall(ident"valid", newDotExpr(match, ident"node")), newStmtList(match)))
    top = level.add(ifelse.add(newNimNode(nnkElse).add(top)))
  newBlockStmt(newEmptyNode(), flow.add(top))


template indentBlock*(state: var AstState, level: uint, body: untyped): untyped =
  # Warn! Body should not have return
  let backup = state.indent
  state.indent = level
  body
  state.indent = backup


## Pair tree building utilities

# todo: better names, lol
type
  PairListBuilder* = object
    bottom*: int    # Points towards bottom-most allocated node
    node*: AstNode  # Node for which tree is built

func push*(a: var PairListBuilder, s: var AstState, n: sink AstNode) {.inline.} =
  if a.node.kind == nkNone:
    a.node = n
  elif a.node.kind != nkPair:
    a.node = initAst(nkPair, s.emplace(a.node), s.emplace(n))
    a.bottom = a.node.right
  else:
    s.nodes[a.bottom] = initAst(nkPair, s.emplace(s.nodes[a.bottom]), s.emplace(n))
    a.bottom = s.nodes[a.bottom].right

template push*(a: var PairListBuilder, s: var AstState, kind: AstKind, left, right, head = 0, tail: int = 0): untyped =
  a.push(s, initAst(kind, left, right, head, tail))

template push*(a: var PairListBuilder, s: var AstState, kind: AstKind, n: AstNode): untyped =
  a.push(s, initAst(kind, n.left, n.right, n.head, n.tail))


type
  PairSeqBuilder* = object
    ## Builder for pair-like nodes that have sequences as their sides
    bottom*: int    # Points towards bottom-most allocated node
    node*: AstNode  # Node for which tree is built

func initPairSeqBuilder(kind: AstKind): PairSeqBuilder {.inline.} =
  result.node.kind = kind

func push*(a: var PairSeqBuilder, s: var AstState, n: sink AstNode) {.inline.} =
  if a.node.left == EmptyIndex:
    a.node.left = s.emplace(n)
  elif a.node.right == EmptyIndex:
    a.node.right = s.emplace(n)
    a.bottom = a.node.right
  else:
    s.nodes[a.bottom] = initAst(nkPair, s.emplace(s.nodes[a.bottom]), s.emplace(n))
    a.bottom = s.nodes[a.bottom].right

template push*(a: var PairSeqBuilder, s: var AstState, kind: AstKind, left, right, head = 0, tail: int = 0): untyped =
  a.push(s, initAst(kind, left, right, head, tail))

template push*(a: var PairSeqBuilder, s: var AstState, kind: AstKind, n: sink AstNode): untyped =
  a.push(s, initAst(kind, n.left, n.right, n.head, n.tail))

func consumeToPairSeq*(s: var AstState, kind: AstKind, def: GrammarDef, start: int): GrammarRet =
  result.future = start
  var builder = initPairSeqBuilder(kind)
  var match = s.def(result.future)
  while match.node.valid:
    builder.push(s, match.node)
    result.future = match.future
    match = s.def(result.future)
  result.node = builder.node

import lexer

type
  AstKind* = enum
    akInvalid,
    akStmt,
    akExpr,
    akIdent,
    akKeyword,
    akInt,
    akFloat,
    akString,
    akStmtList,
    akParamList,
    akList,
    akProc,
    akAssign,
    akDefinition,

  AstNode* = object
    kind*: AstKind
    cursor*: int    # Position in source
    size*: int      # Size starting from cursor position
    # Each side holds an offset in shared node sequence
    # By limiting each node to only hold certain amount of other nodes
    # we can potentially don't allocate any extra heap data
    left*: int
    right*: int

  AstState* = object
    # Main idea behind this implementation is to achieve cache locality
    # by utilizing single buffer for all ast nodes and make them reference
    # each other by buffer offset instead of pointers to heap allocated space
    nodes*: seq[AstNode]
    tokens: seq[Token]
    cursor*: int # Position in tokens
    astfun*: proc(s: var AstState)
    inplace*: seq[AstNode]

  GrammarError* = object of CatchableError


func initAstState*(toks: seq[Token], fn: proc(s: var AstState)): AstState =
  AstState(tokens: toks, astfun: fn)


proc parse*(s: var AstState): bool =
  try:
    s.astfun(s)
    result = true
  except GrammarError as err:
    echo "Grammar error: ", err.msg


func atEnd*(s: AstState): bool =
  s.cursor <= s.tokens.len - 1

import ast, lexer
export ast


# todo: alternative idea: pass cursor advancement as mutable shared int
#       this way can be achieved locality without storing ghost state in each stage
#       and in general when working with branches it's preferable to not depend
#       on single data i.e. AstState instance
#       -- this requires usage of ptr that points to stack memory which is unsafe
#          we have to make sure that passed pointer can't be saved, otherwise
#          it's guaranteed by call nature that passed ptr always alive
#          -- maybe we can define some type that prohibit assignment and distinct?
#       -- mutability isn't necessary, we could just make that each def must receive
#          starting position and return advancement if that happened

# todo: make node children as sequences within the buffer, ideally neighboring
#       the node itself and just storing seq size for bound checking
#       -- main problem with this that recursive node building requires
#          some temporary storage to build children consequently,
#          otherwise buffer will be segmented by nodes of different trees

# todo: maybe pass only 'var seq[AstNode]' instead of AstState? as rules should only
#       use state for emplacing children

# todo: maybe errors should return their start too? can be convenient

# todo: macro system for implementation of rules, otherwise it's extremely bug prone and monotonous
## example:
# astRule: "list"
#   nkList:
#     nkListOpen
#     left <- *expr
#     nkListClose

# todo: consumer object for pushing stuff into pair trees programmatically


# todo: way of standardized forward declaration by just names
proc astExpr(s: var AstState, start: int): GrammarRet {.nimcall, noSideEffect, raises: [GrammarError], gcsafe.}
proc astList(s: var AstState, start: int): GrammarRet {.nimcall, noSideEffect, raises: [GrammarError], gcsafe.}
proc astDef(s: var AstState, start: int): GrammarRet {.nimcall, noSideEffect, raises: [GrammarError], gcsafe.}
proc astAssign(s: var AstState, start: int): GrammarRet {.nimcall, noSideEffect, raises: [GrammarError], gcsafe.}
proc astStmtList(s: var AstState, start: int): GrammarRet {.nimcall, noSideEffect, raises: [GrammarError], gcsafe.}
proc astStmtListScope(s: var AstState, start: int): GrammarRet {.nimcall, noSideEffect, raises: [GrammarError], gcsafe.}


proc consumePairs(s: var AstState, def: GrammarDef, start: int): GrammarRet =
  ## Generic pair constructor that packs successful calls into nkPair tree
  result = s.def(start)
  if result.node.valid:
    # if second match - construct pair of two matches
    let second = s.def(result.future)
    if second.node.valid:
      result.future = second.future
      result.node = AstNode(
        kind: nkPair,
        left: s.emplace(result.node),
        right: s.emplace(second.node))
      var head = result.node.right
      for token in s.stream(second.future):
        # all consequent matches work on right sides of most bottom current pair
        let other = s.def(result.future)
        if other.node.valid:
          result.future = other.future
          s.nodes[head] = AstNode(
            kind: nkPair,
            left: s.emplace(s.nodes[head]),
            right: s.emplace(other.node))
          head = s.nodes[head].right
        else: break


proc astExpr(s: var AstState, start: int): GrammarRet =
  ## +(ident|int|string|list)
  template setPrimitive(k: AstKind) =
    result.node.kind = k
    result.node.head = s[start].head
    result.node.tail = s[start].tail
    result.future = start + 1

  case s[start].kind
  of tkIdent: setPrimitive nkIdent
  of tkInt: setPrimitive nkInt
  of tkString: setPrimitive nkString
  of tkListOpen: result = s.astList(start)
  else: result.node.kind = nkError


proc astList(s: var AstState, start: int): GrammarRet =
  ## [?expr *expr]
  ## left side <- elems of list
  result.node.kind = nkError
  let ghost = s.nodes.len
  if s[start].kind == tkListOpen:
    let exprs = s.consumePairs(astExpr, start + 1)
    if exprs.node.valid:
      if s[exprs.future].kind == tkListClose:
        result.future = exprs.future + 1
        result.node.kind = nkList
        result.node.left = s.emplace(exprs.node)
      else:
        # call to consumePairs can allocate nodes, so we should clear them if list wasn't constructed
        s.letgo ghost
    elif s[start + 1].kind == tkListClose:
        result.future = start + 2
        result.node.kind = nkList


proc astDef(s: var AstState, start: int): GrammarRet =
  ## ident(name) ident(type) *ident(modifiers)
  ## left side <- type specifier
  ## todo: right side <- list of modifiers
  result.node.kind = nkError
  if s[start].kind == tkIdent and s[start + 1].kind == tkIdent:
    result.node.kind = nkDef
    result.node.head = s[start].head
    result.node.tail = s[start].tail
    result.node.left = s.emplace(
      AstNode(
        kind: nkIdent,
        head: s[start + 1].head,
        tail: s[start + 1].tail))
    result.future = start + 2


# todo: operating on previous stored node is troublesome, it might be better
#       to require calling of depending functions from some middle ground
proc astAssign(s: var AstState, start: int): GrammarRet =
  ## >prev(variable) = stmtList | expr
  ## left side <- settable / variable
  ## right side <- setting / value
  result.node.kind = nkError
  let prev = s.nodes.high
  # echo s.nodes[prev].kind
  if s[start].kind == tkAssign and s.nodes[prev].kind != nkNone:
    var variant: GrammarRet
    variant = s.astExpr(start + 1)
    if not variant.node.valid:
      variant = s.astStmtListScope(start + 1)
    if variant.node.valid:
      result.node = AstNode(
        kind: nkAssign,
        left: prev,
        right: s.emplace variant.node)
      result.future = variant.future


proc astStmtListScope(s: var AstState, start: int): GrammarRet =
  discard


proc astStmtList(s: var AstState, start: int): GrammarRet =
  discard


const astModule* = [astDef]

import ast, lexer
export ast

# todo: make node children as sequences within the buffer, ideally neighboring
#       the node itself and just storing seq size for bound checking
#       -- main problem with this that recursive node building requires
#          some temporary storage to build children consequently,
#          otherwise buffer will be segmented by nodes of different trees

# todo: maybe pass only 'var seq[AstNode]' instead of AstState? as rules should only
#       use state for emplacing children

# todo: maybe errors should return their start too? can be more than just convenient

# todo: macro system for implementation of rules, otherwise it's extremely bug prone and monotonous
# example:
# astRule assign:
#   variant(left: rule.ident, tkAssign, right: rule.expr):
#     Token(kind: nkAssign, left: left, right: right)
#   variant(left: rule.list, tkAssign, right: rule.expr):
#     Token(kind: nkAssign, left: left, right: right)

# astRule list:
#   variant(tkListOpen, pairs: sequence.expr, tkListClose):
#     Token(kind: nkList, left: pairs.left, right: pairs.right)


# todo: maybe we can make error handling without using exception, but with regular function flow?


# todo: way of standardized forward declaration by just names
func astExpr(s: var AstState, start: int): GrammarRet {.nimcall, noSideEffect, raises: [GrammarError], gcsafe.}
func astList(s: var AstState, start: int): GrammarRet {.nimcall, noSideEffect, raises: [GrammarError], gcsafe.}
func astDef(s: var AstState, start: int): GrammarRet {.nimcall, noSideEffect, raises: [GrammarError], gcsafe.}
func astIfExpr(s: var AstState, start: int): GrammarRet {.nimcall, noSideEffect, raises: [GrammarError], gcsafe.}
func astBody(s: var AstState, start: int): GrammarRet {.nimcall, noSideEffect, raises: [GrammarError], gcsafe.}
func astColonBody(s: var AstState, start: int): GrammarRet {.nimcall, noSideEffect, raises: [GrammarError], gcsafe.}
func astAssign(s: var AstState, start: int): GrammarRet {.nimcall, noSideEffect, raises: [GrammarError], gcsafe.}


func astExpr(s: var AstState, start: int): GrammarRet =
  ## ident | int | string | list
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
  else: discard


func astIfExpr(s: var AstState, start: int): GrammarRet =
  ## if expr :body *(elif expr :body) ?(else :body)
  var
    branches: PairListBuilder
    exprMatch: GrammarRet
    bodyMatch: GrammarRet

  if s[start].kind == tkIf:
    debugEcho start, s[start + 2].kind
    exprMatch = s.astExpr(start + 1)
    if exprMatch.node.valid:
      bodyMatch = s.astColonBody(exprMatch.future)
      if bodyMatch.node.valid:
        branches.push(s, initAst(nkIfBranch, s.emplace(exprMatch.node), s.emplace(bodyMatch.node)))
        # consume all elif branches
        while s[bodyMatch.future].kind == tkElif:
          exprMatch = s.astExpr(bodyMatch.future + 1)
          if exprMatch.node.valid:
            bodyMatch = s.astColonBody(exprMatch.future)
            if bodyMatch.node.valid:
              branches.push(s, initAst(nkElifBranch, s.emplace(exprMatch.node), s.emplace(bodyMatch.node)))
            else: raiseGrammarError("'elif' should be followed by colon body")
          else: raiseGrammarError("'elif' should have expression attached")
        # optional else branch
        if s[bodyMatch.future].kind == tkElse:
          bodyMatch = s.astColonBody(bodyMatch.future + 1)
          if bodyMatch.node.valid:
            branches.push(s, nkElseBranch, bodyMatch.node)
          else: raiseGrammarError("'else' should be followed by colon body")
        # result node is pair tree of branches
        result.node = branches.node
        result.node.kind = nkIfExpr
        result.future = bodyMatch.future
    else: raiseGrammarError("'if' should have expression attached")


func astList(s: var AstState, start: int): GrammarRet =
  ## [?expr *expr]
  ## left side <- elems of list
  let ghost = s.nodes.len
  if s[start].kind == tkListOpen:
    let exprList = s.consumeToPairSeq(nkList, astExpr, start + 1)
    if exprList.node.valid:
      if s[exprList.future].kind == tkListClose:
        result.future = exprList.future + 1
        result.node = exprList.node
      else:
        # call to consumeToPairSeq can allocate nodes, so we should clear them if list wasn't constructed
        s.letgo ghost
    elif s[start + 1].kind == tkListClose:
        result.future = start + 2
        result.node.kind = nkList


func astDef(s: var AstState, start: int): GrammarRet =
  ## ident(name) ident(type) *ident(modifiers)
  ## left side <- type specifier
  ## right side <- initializing value
  if s[start].kind == tkIdent and s[start + 1].kind == tkIdent:
    result.node.kind = nkDef
    result.node.head = s[start].head
    result.node.tail = s[start].tail
    result.node.left = s.emplace(AstNode(kind: nkIdent, head: s[start + 1].head, tail: s[start + 1].tail))
    result.future = start + 2
    if s[start + 2].kind == tkAssign:
      let match = s.astExpr(start + 3)
      if match.node.valid:
        result.node.right = s.emplace match.node
        result.future = match.future


func astAssign(s: var AstState, start: int): GrammarRet =
  ## ident = (expr | ifExpr)
  ## left side <- ident
  ## right side <- expr
  if s[start].kind == tkIdent and s[start + 1].kind == tkAssign:
    # let match = s.astExpr(start + 2)
    let match = s.ruleAny(start + 2, astExpr, astIfExpr)
    if match.node.valid:
      result.node.kind = nkAssign
      result.node.left = s.emplace AstNode(kind: nkIdent, head: s[start].head, tail: s[start].tail)
      result.node.right = s.emplace match.node
      result.future = match.future


func astColonBody(s: var AstState, start: int): GrammarRet =
  ## New body scope that is opened by colon, newline and new level of indentation
  if s[start].kind == tkColon and s[start + 1].kind == tkNewIndent:
    if s[start + 1].vUint == s.indent + 1:
      s.indent.inc
      result = s.astBody(start + 2)


# todo: SHOULD BE TOTALLY REDONE
# todo: fix bug with inability to leave function on invalid input
func astBody(s: var AstState, start: int): GrammarRet =
  ## +(astIfExpr | astDef | astAssign | astExpr)
  template matchVariant(rule: GrammarDef) =
    let match = s.rule(cursor)
    if match.node.valid:
      builder.push(s, match.node)
      cursor = match.future
      let cur = s[match.future]
      if not cur.valid: break # eof
      elif cur.kind == tkNewIndent:
        if cur.vUint < indent:
          s.indent = cur.vUint # todo: it's really strange to set indent from within rule
          cursor.inc
          break
        else:
          raiseGrammarError("invalid indentation within body")
      elif cur.kind == tkNewline: # nested scopes delete newlines, so, we have to check
        cursor.inc
      elif match.node.kind in ScopedNodes: discard
      else: break
      continue # start new matching round

  var
    builder: PairSeqBuilder
    cursor = start
  let indent = s.indent
  while true:
    matchVariant astIfExpr
    matchVariant astDef
    matchVariant astAssign
    matchVariant astExpr
    if not s.isEnd(cursor):
      raiseGrammarError("unexpected token " & $s[cursor].kind & " at position " & $cursor)
    else: break

  if builder.node.left != EmptyIndex:
    # debugEcho s.nodeToString(builder.node)
    # result.node = AstNode(kind: nkBody, left: builder.node.left, right: builder.node.right)
    result.node = initAst(nkBody, builder.node.left, builder.node.right)
    result.future = cursor


const astModule* = [astBody]

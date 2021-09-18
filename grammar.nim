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


# todo: shorten it
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
    exprMatch = s.astExpr(start + 1)
    if exprMatch.node.valid:
      bodyMatch = s.astColonBody(exprMatch.future)
      if bodyMatch.node.valid:
        branches.push(s, initAst(nkElifBranch, s.emplace(exprMatch.node), s.emplace(bodyMatch.node)))
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
      else: raiseGrammarError("'if' should be followed by colon body")
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
      else: raiseGrammarError("list should have closing bracket")
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
    result.node.left = s.emplace(initAst(nkIdent, s[start + 1].head, s[start + 1].tail))
    result.future = start + 2
    if s[start + 2].kind == tkAssign:
      let match = s.astExpr(start + 3)
      if match.node.valid:
        result.node.right = s.emplace match.node
        result.future = match.future


# todo: it should use ifExprInline for possible ternary
func astAssign(s: var AstState, start: int): GrammarRet =
  ## ident = (expr | ifExpr)
  ## left side <- ident
  ## right side <- expr
  if s[start].kind == tkIdent and s[start + 1].kind == tkAssign:
    let match = s.ruleAny(start + 2, astIfExpr, astExpr)
    if match.node.valid:
      result.node.kind = nkAssign
      result.node.left = s.emplace initAst(nkIdent, s[start].head, s[start].tail)
      result.node.right = s.emplace match.node
      result.future = match.future


func astColonBody(s: var AstState, start: int): GrammarRet =
  if s[start].kind == tkColon and
      s[start + 1].kind == tkNewIndent and
      s[start + 1].vUint == s.indent + 1:
    indentBlock s, s[start + 1].vUint:
      result = s.astBody(start + 2)

func astBody(s: var AstState, start: int): GrammarRet =
  ## +(astIfExpr | astDef | astAssign | astExpr)
  template astBodyRules(state: var AstState, cursor: int): untyped =
    state.ruleAny(cursor, astIfExpr, astDef, astAssign, astExpr)

  var builder: PairSeqBuilder
  var match = s.astBodyRules(start)

  while match.node.valid:
    result.future = match.future
    builder.push(s, match.node)
    if match.node.kind notin ScopedNodes: 
      case s[match.future].kind
      of tkNewline: discard
      of tkEndOfFile: break
      of tkNewIndent:
        if s[match.future].vUint < s.indent: break
        elif s[match.future].vUint == s.indent: discard
        else: raiseGrammarError("invalid indentation within body")
      else: raiseGrammarError("unexpected token " & $s[match.future].kind & " at position " & $match.future)
    match = s.astBodyRules(match.future + 1)

  if builder.node.left != EmptyIndex:
    result.node = initAst(nkBody, builder.node.left, builder.node.right)

func astIndent(s: var AstState, start: int): GrammarRet =
  # Kinda hacky way to deal with trailing indents
  if s[start].kind == tkNewIndent:
    s.indent = s[start].vUint
    result.node.kind = nkDontEat
    result.future = start + 1


const astModule* = [astIndent, astBody]

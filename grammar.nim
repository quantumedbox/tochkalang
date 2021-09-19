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

# todo: error-stack for unrolling calls and their states


{.push nimcall, noSideEffect, raises: [GrammarError].}
func astBody(s: var AstState, start: int): GrammarRet
func astColonBody(s: var AstState, start: int): GrammarRet
func astList(s: var AstState, start: int): GrammarRet
func astNamedTuple(s: var AstState, start: int): GrammarRet
{.pop.}


const astModule* = [astBody] # todo: make single main function that parses module, there's no need for multiple


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
    branches: PairSeqBuilder
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
        result.node = initAst(nkIfExpr, branches.node.left, branches.node.right)
        result.future = bodyMatch.future
      else: raiseGrammarError("'if' should be followed by colon body")
    else: raiseGrammarError("'if' should have expression attached")


func astList(s: var AstState, start: int): GrammarRet =
  ## [?expr *expr]
  ## left side <- elems of list
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


func astInit(s: var AstState, start: int): GrammarRet =
  ##   = expr
  ## | = indentblock => body
  if s[start].kind == tkAssign:
    if s[start + 1].kind == tkNewIndent and
        s[start + 1].vUint == s.indent + 1:
      indentBlock s, s[start + 1].vUint:
        let match = s.astBody(start + 2)
        if match.node.valid:
          result.node = initAst(nkInit, s.emplace match.node, 0)
          result.future = match.future
    else:
      let match = s.astExpr(start + 1)
      if match.node.valid:
        result.node = initAst(nkInit, s.emplace match.node, 0)
        result.future = match.future


func astDef(s: var AstState, start: int): GrammarRet =
  ## ident(name) ident(type) ?:namedTuple ?init
  ## left side <- type specifier
  ## right side <- initialization list
  if s[start].kind == tkIdent and s[start + 1].kind == tkIdent:
    let left = initAst(nkIdent, head = s[start + 1].head, tail = s[start + 1].tail)
    result.node = initAst(nkDef, s.emplace left, 0, s[start].head, s[start].tail)
    result.future = start + 2
    var builder: PairListBuilder
    block tupleDef:
      let match = s.astNamedTuple(result.future)
      if match.node.valid:
        builder.push(s, match.node)
        result.future = match.future
    block initDef:
      debugecho result.future
      let match = s.astInit(result.future)
      debugecho match
      if match.node.valid:
        builder.push(s, match.node)
        result.future = match.future
    if builder.node.valid:
      result.node.right = s.emplace builder.node


# todo: it should use ifExprInline for possible ternary
func astAssign(s: var AstState, start: int): GrammarRet =
  ## ident = (expr | ifExpr)
  ## left side <- ident
  ## right side <- expr
  if s[start].kind == tkIdent and s[start + 1].kind == tkAssign:
    let match = s.ruleAny(start + 2, [astIfExpr, astExpr])
    if match.node.valid:
      result.node.kind = nkAssign
      result.node.left = s.emplace initAst(nkIdent, s[start].head, s[start].tail)
      result.node.right = s.emplace match.node
      result.future = match.future


func astBracketCall(s: var AstState, start: int): GrammarRet =
  ## ident(callee) list(unnamed args)
  if s[start].kind == tkIdent and s[start + 1].kind == tkListOpen:
    let left = s.emplace initAst(nkIdent, head = s[start].head, tail = s[start].tail)
    let listMatch = s.astList(start + 1)
    result.node = initAst(nkCall, left, s.emplace listMatch.node)
    result.future = listMatch.future

func astCall(s: var AstState, start: int): GrammarRet =
  ## astBracketCall
  s.ruleAny(start, [astBracketCall])


func astColonBody(s: var AstState, start: int): GrammarRet =
  ## indentblock => body
  if s[start].kind == tkColon and
      s[start + 1].kind == tkNewIndent and
      s[start + 1].vUint == s.indent + 1:
    indentBlock s, s[start + 1].vUint:
      result = s.astBody(start + 2)

func astBody(s: var AstState, start: int): GrammarRet =
  ## +(astIfExpr | astDef | astAssign | astCall | astExpr)
  template astBodyRules(state: var AstState, cursor: int): untyped =
    state.ruleAny(cursor, [astIfExpr, astDef, astAssign, astCall, astExpr])

  var builder: PairSeqBuilder
  var match = s.astBodyRules(start)
  while match.node.valid:
    result.future = match.future
    builder.push(s, match.node)
    # if nested indented block was emplaced - caller can't see newline to prove continuity
    if match.node.kind notin ScopedNodes:
      case s[match.future].kind
      # each statement should be divided by newline
      of tkNewline: match.future.inc
      of tkEndOfFile: break
      of tkNewIndent:
        # if encountered change of indent that is lower than current - end body
        if s[match.future].vUint < s.indent:
          result.future.inc
          break
        elif s[match.future].vUint > s.indent:
          raiseGrammarError("invalid indentation within body, token " & $match.future)
      else: raiseGrammarError("unexpected token " & $s[match.future].kind & " at position " & $match.future)
    match = s.astBodyRules(match.future)

  result.node = initAst(nkBody, builder.node.left, builder.node.right)


func astEnumNamedTuplePair(s: var AstState, start: int): GrammarRet {.inline.} =
  ## ident : expr
  if s[start].kind == tkIdent and s[start + 1].kind == tkColon:
    let left = initAst(nkIdent, head = s[start].head, tail = s[start].tail)
    let right = s.astExpr(start + 2)
    if right.node.valid:
      result.node = initAst(nkNamedTuplePair, s.emplace left, s.emplace right.node)
      result.future = right.future
    else: raiseGrammarError("named tuple pair should have expression attached")

func astEnumNamedTuple(s: var AstState, start: int): GrammarRet = 
  ## +(enumNamedTuplePair newline)
  if s[start].kind == tkColon and
      s[start + 1].kind == tkNewIndent and
      s[start + 1].vUint == s.indent + 1:
    # s.indentBlock s[start + 1].vUint:
    s.indent.inc # todo: might be problematic for other indent-based rules, but some do depend on it
                 # maybe indeed newindents that follow shouldn't be consumed and left to be managed by main rules
    var match = s.astEnumNamedTuplePair(start + 2)
    var builder: PairSeqBuilder
    while match.node.valid:
      builder.push(s, match.node)
      result.future = match.future
      case s[match.future].kind:
      of tkNewline: discard # continue
      of tkNewIndent: # finish on indent closing
        if s[match.future].vUint < s.indent:
          result.future = match.future + 1
          break
        elif s[match.future].vUint > s.indent:
          raiseGrammarError("invalid indentation within tuple definition")
        # todo: will equal indent be possible to encounter?
      of tkEndOfFile: break
      else: break # todo: sometimes we care about assignment that comes after, but in other cases it might be bad
      # else: raiseGrammarError("unexpected token " & $s[match.future].kind & " at position " & $match.future)
      match = s.astEnumNamedTuplePair(match.future + 1)

    result.node = initAst(nkNamedTuple, builder.node.left, builder.node.right)

func astNamedTuple(s: var AstState, start: int): GrammarRet =
  ## astEnumNamedTuple
  result = s.ruleAny(start, [astEnumNamedTuple])

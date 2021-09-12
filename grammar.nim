import ast, lexer
export ast


# todo: alternative idea: pass cursor advancement as mutable shared int
#       this way can be achieved locality without storing ghost state in each stage
#       and in general when working with branches it's preferable to not depend
#       on single data i.e. AstState instance


proc astExpr(s: var AstState): GrammarRet
proc astList(s: var AstState): GrammarRet


proc consumePairs[T: GrammarDef](s: var AstState, def: T): GrammarRet =
  ## Generic pair constructor that packs successful calls into nkPair tree
  let ghost = s.catchGhost
  result = s.def
  if result.node.valid:
    let second = s.def
    if second.node.valid:
      result.node = AstNode(
        kind: nkPair,
        left: s.emplace(result.node),
        right: s.emplace(second.node)
      )
      var head = result.node.right
      for token in s.stream:
        let other = s.def
        if other.node.valid:
          s.nodes[head] = AstNode(
            kind: nkPair,
            left: head,
            right: s.emplace(other.node)
          )
          head = s.nodes[head].right
        else: break
  else:
    s.letgo ghost


# todo: could introduce some shortcuts to eliminate repetition
proc astExpr(s: var AstState): GrammarRet =
  ## +(ident|int|string|list)
  result.node.kind = nkError
  case s.current.kind
  of tkIdent:
    result.node.kind = nkIdent
    result.node.head = s.current.head
    result.node.tail = s.current.tail
    s.advance
  of tkInt:
    result.node.kind = nkInt
    result.node.head = s.current.head
    result.node.tail = s.current.tail
    s.advance
  of tkString:
    result.node.kind = nkString
    result.node.head = s.current.head
    result.node.tail = s.current.tail
    s.advance
  of tkListOpen:
    result = s.astList
  else: discard


# todo: restoring cursor by hand can be annoying, can probably somehow fix it
#       needs some shortcuts or automation
proc astList(s: var AstState): GrammarRet =
  ## '['*expr']'
  result.node.kind = nkList
  let ghost = s.catchGhost
  if s.current.kind == tkListOpen:
    s.advance
    let exprs = s.consumePairs astExpr
    if s.current.kind == tkListClose:
      s.advance
      if exprs.node.valid:
        result.node.left = s.emplace exprs.node
        result.future = s.cursor - ghost.cursor
    else:
      s.letgo ghost
      result.node.kind = nkError


const astModule* = [astList]

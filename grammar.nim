import ast

# todo: how to pass eaten tokens on successful match? storing them in AstNode doesn't seem right,
#       returning tuple of (AstNode, future) is better, but i'm not sure


func parseModule*(s: var AstState) =
  while not s.atEnd:
    if true: break # todo
    else:
      raise newException(GrammarError, "unknown grammar")

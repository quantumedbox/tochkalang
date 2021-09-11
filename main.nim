import lexing, grammar

let src = stdin.readAll
var p = initLexer(src, lexModule)
if p.tokenize:
  echo p

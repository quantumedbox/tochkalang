import lexing, grammar

let src = stdin.readAll
let x = tokenize(src, lexModule)
if x.atEnd:
  echo x

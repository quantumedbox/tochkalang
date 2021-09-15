import lexing, grammar

let src = stdin.readAll
let x = tokenize(src, lexModule)
if x.tokens.len <= 50:
  echo x
else:
  echo "too many tokens to sanely print..."
if x.atEnd:
  let s = parse(x, astModule)
  echo s

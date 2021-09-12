import lexing, grammar

let src = stdin.readAll
let x = tokenize(src, lexModule)
if x.atEnd:
  if x.tokens.len <= 50:
    echo x
  else:
    echo "too many tokens to sanely print..."
let s = parse(x, astModule)
echo s


# todo: allocation doubling sequences might be more than enough, no need for pages

const GrowRatio = 2

type
  RetainSeq*[T] = object
    ## Sequence that grows doubling its capacity and preserving indexes of items
    data: seq[T]
    cap: int
    vacant: seq[int]

func push*[T](s: RetainSeq[T], v: sink T): int =
  if s.cap == s.data.len:
    let newCap = s.data.len * GrowRatio
    s.data.setLen(newCap)
    s.cap = newCap
  if s.vacant.len == 0:
    s.data.add v
    s.data.high
  else:
    let spot = s.vacant.high
    s.data[spot] = v
    s.vacant.setLen(spot)
    spot

{.push inline.}

func `[]`*[T](s: RetainSeq[T], idx: int): lent T =
  result = s.data[idx]

func erase*[T](s: RetainSeq[T], idx: int) =
  s.vacant.add idx

{.pop inline.}

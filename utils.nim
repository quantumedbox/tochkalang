
type
  Id* = distinct uint
  RetainSeq*[T: typed, P: Natural = 128u] = object
    ## Structure that preserves indexes of holding objects
    ## and stores them in constant size pages
    ## Warn! After deletion id of old objects shouldn't be used to access it
    ##       make sure that it isn't possible
    data: seq[array[P, T]] # sequence of page arrays
    size: uint
    vacant: seq[uint] # Free spots in underlying data


{.push inline.}

func add*[T, P](s: RetainSeq[T, P], v: sink T): Id =
  if s.vacant.len > 0:
    let spot = s.vacant[s.vacant.high]
    let page = spot div P
    let idx = spot mod P
    s.data[page][idx] = v
    s.vacant.setLen s.vacant.high
    spot
  else:
    let page = s.size div P
    let idx = s.size mod P
    if idx == 0:
      s.data.add default[array[P, T]]()
    s.data[page][idx] = v
    result = s.size
    s.size.inc

# todo: should it bound check?
func `[]`*[T, P](s: RetainSeq[T, P], i: Id): T =
  s.data[s.size div i][s.size mod i]

# todo: should it bound check?
func erase*[T, P](s: RetainSeq[T, P], i: Id) =
  s.vacant.add i

{.pop.}

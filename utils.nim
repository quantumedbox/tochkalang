
type
  Id* = distinct uint
  RetainSeqSized*[I: static[uint], T] = object
    ## Structure that preserves indexes of holding objects
    ## and stores them in constant size pages
    ## Warn! After deletion id of old objects shouldn't be used to access it
    ##       make sure that it isn't possible
    data: seq[ptr array[I, T]] # sequence of page arrays
    size: uint
    vacant: seq[Id] # free spots in underlying data

  # PageSeqSized*[I: static[uint], T] = object
  #   ## Sequence of paged data for chunk allocation
  #   data: seq[ptr array[I, T]]
  #   size: uint

  RetainSeq*[T] = RetainSeqSized[128u, T]
  # PageSeq*[T] = PageSeqSized[128u, T]


proc push*[I, T](s: var RetainSeqSized[I, T], v: T): Id =
  if s.vacant.len > 0:
    let spot = s.vacant[s.vacant.high]
    let page = spot.uint div I
    let idx = spot.uint mod I
    s.data[page][idx] = v
    s.vacant.setLen s.vacant.high
    result = spot.Id
  else:
    let page = s.size div I
    let idx = s.size mod I
    if idx == 0:
      s.data.add createU(array[I, T])
    s.data[page][idx] = v
    result = s.size.Id
    s.size.inc

{.push inline.}

# todo: should it bound check?
func `[]`*[I, T](s: var RetainSeqSized[I, T], i: Id): T =
  s.data[i.uint div I][i.uint mod I]

# todo: should it bound check?
func erase*[I, T](s: var RetainSeqSized[I, T], i: Id) =
  s.vacant.add i

{.pop.}

func `=destroy`[I, T](s: var RetainSeqSized[I, T]) =
  for p in s.data:
    dealloc(p)

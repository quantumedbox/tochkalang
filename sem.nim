import utils
export utils

type
  SemDef* = object
    nameHead: int
    nameTail: int
    initExpr: Id # SemExpr or nothing
  SemExpr* = object

  SemState* = object
    ## Semantic state handler that encapsulates structs of certain types in structure-of-array fashion
    source: string
    defs: RetainSeq[SemDef]
    exprs: RetainSeq[SemExpr]

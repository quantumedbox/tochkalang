import std/tables
import ast, utils
export utils

type
  SemDef* = object
    name: int         # nkIdent in nodes
    semType: Id       # SemType
    initExpr: Id      # SemExpr or nothing
  SemTypeKind* = enum
    stInt,
    stString,
    stSequence,
  SemType* = object
    name: string      # 
    base: SemTypeKind # inheritance from built-in type
  SemExpr* = object
    semType: Id       # SemType
  SemScope* = object
  SemState* = object
    ## Semantic state handler that encapsulates structs of certain types in structure-of-array fashion
    source: string
    nodes: seq[AstNode]

    types: RetainSeq[SemType]
    typeLookup: Table[string, Id] # name to SemType

    defs: RetainSeq[SemDef]
    exprs: RetainSeq[SemExpr]

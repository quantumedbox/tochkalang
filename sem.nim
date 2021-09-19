import std/tables
import ast, utils
export utils

# Semantic checks are implemented by annotating existing ast trees

# todo: check for if-elif-else returning value on all possible paths and with right types 
# todo: check expression types on initialization and assignments
# todo: check for existence of identifiers within current scope


type
  SemDef* = object
    name: int         # nkIdent in nodes
    semType: int      # SemType
    initExpr: int     # SemExpr or nothing
  SemTypeKind* = enum
    stVoid,           # Meta type that is expressed by absence
    stInt,
    stString,
    stSequence,
  SemType* = object
    name: string      # 
    base: SemTypeKind # type implementation
  SemExpr* = object
    semType: int      # SemType of evaluation
    value: int
  SemScope* = object
    parent: int       # SemScope from which this scope inherit from, can be 0 for main
    defLookup: Table[string, int] # local definitions
  SemState* = object
    ## Semantic state handler that encapsulates semantics objects in structure-of-array fashion
    source: string
    nodes: seq[AstNode]

    types: RetainSeq[SemType]
    typeLookup: Table[string, int] # name to SemType

    defs: RetainSeq[SemDef]
    exprs: RetainSeq[SemExpr]

import std/tables
import ast, utils
export utils

# todo: check for if-elif-else returning value on all possible paths and with right types 
# todo: check expression types on initialization and assignments
# todo: check for existence of identifiers within current scope


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
    base: SemTypeKind # type implementation
  SemExpr* = object
    semType: Id       # SemType of evaluation
    value: Id
  SemScope* = object
    parent: Id        # SemScope from which this scope inherit from, can be 0 for main
    defLookup: Table[string, Id] # local definitions
  SemState* = object
    ## Semantic state handler that encapsulates semantics objects in structure-of-array fashion
    source: string
    nodes: seq[AstNode]

    types: RetainSeq[SemType]
    typeLookup: Table[string, Id] # name to SemType

    defs: RetainSeq[SemDef]
    exprs: RetainSeq[SemExpr]

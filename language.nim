# Shared language definitions, types and consts

type
  TokenKind* = enum
    tkError,                    # Signal for caller that parsing wasn't successful
    tkIdent,                    # Literal symbol
    tkValue,                    # Any value that is known without context
    tkKeyword,                  # Literal symbol protected from being an tkIdent
    tkDot,                      # '.'
    tkAssign,                   # '='
    tkColon,                    # Used for opening scopes and specifying return type
    tkListOpen, tkListClose,    # '[', ']'
    tkNewline,                  # {'\n', '\r'}
    tkNewIndent,                # Change of indentation level

const
  EndChar* = '\0' # todo: not sure about char choise, maybe unicode has something for that
  IndentChar* = ' '
  IndentTemplate* = "  "
  SpecialCharTokens* = {tkDot, tkAssign, tkColon}

  Keywords* = ["proc", "scope", "mut", "type", "cond"] # should only consist of isLetter chars
  ExportMarker* = '*'
  StringMarkers* = {'"'}

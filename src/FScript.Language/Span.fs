namespace FScript.Language

type Position =
    { File: string option
      Line: int
      Column: int }

type Span = { Start: Position; End: Position }

module Span =
    let inline pos line col = { File = None; Line = line; Column = col }
    let inline posInFile file line col = { File = Some file; Line = line; Column = col }
    let inline mk startPos endPos = { Start = startPos; End = endPos }
    let merge (a: Span) (b: Span) = { Start = a.Start; End = b.End }

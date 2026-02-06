namespace FScript.Core

type ParseError = { Message: string; Span: Span }
type TypeError = { Message: string; Span: Span }
type EvalError = { Message: string; Span: Span }

exception ParseException of ParseError
exception TypeException of TypeError
exception EvalException of EvalError

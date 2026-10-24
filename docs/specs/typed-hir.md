# Typed High-Level IR (HIR)

## Status and purpose

This document specifies the high-level intermediate representation produced by `FScript.Compiler`. HIR is the first compiler-owned form after parsing, name resolution, and Hindley–Milner inference. It is fully resolved, fully typed, platform-neutral, and still preserves FScript language structure.

HIR is an internal compiler contract. It is not the `.fsir` artifact format and has no compatibility guarantee between FScript versions.

The public construction path is:

```text
Program
  -> TypeInfer.inferResolvedProgramWithExterns
  -> HirLowering.lower
  -> HirValidation.validate
  -> Hir.Program
```

Callers normally use `Compiler.compile`, which performs this pipeline and also produces validated LIR.

## Program model

`Hir.Program` contains, in source-semantic order:

- `Types`: named record and union declarations;
- `ImportedRecords`: backend-imported record representation declarations;
- `Declarations`: top-level values and functions;
- `Entry`: the optional final top-level expression.

Includes are resolved before HIR construction. One `Hir.Program` therefore represents the complete source program rather than one source file.

Every type, declaration, parameter, pattern binding, local binding, builtin, intrinsic, and extern reference uses a deterministic program-local `SymbolId`. IDs identify declarations; names are retained for diagnostics and readable debug output but never define binding identity.

## Type information

Every `Hir.Expression` and `Hir.Pattern` carries:

- its final instantiated FScript `Type`;
- its originating `Span`;
- a semantic node.

Top-level declarations additionally retain their normalized `Scheme`. Type-variable numbering is deterministic and independent of process-global inference counters.

HIR preserves:

- primitive, tuple, list, option, map, function, task, record, union, named, and type-token types;
- named type identity;
- structural record shapes;
- recursive type references;
- polymorphic declaration schemes;
- explicit named/structural record conversion evidence through `RecordConvert`.

A backend must not repeat FScript compatibility checking or infer structural conversions from its host representation.

## Declarations and metadata

A value declaration contains its symbol ID, source name, normalized scheme, recursive/export flags, structured attributes, span, and value expression.

`Export` metadata records the exported source name and attribute span. Other attributes retain their name, ordered string arguments, and span.

A type declaration contains its symbol ID, name, recursion flag, complete semantic definition, attributes, optional import metadata, and span. Imported records remain ordinary FScript record types; import metadata changes their backend representation, not their FScript typing rules.

## Expressions

HIR directly represents:

- constants and resolved references;
- lambdas, application, and value-first pipelines;
- `if`, local `let`, recursive bindings, and mutually recursive groups;
- resolved primitive operations;
- tuples, records, record updates, explicit record conversions, and field access;
- lists, options, maps, map spread, ranges, cons, append, and indexing;
- declared union construction;
- interpolation, `raise`, `for`, `typeof`, and matches;
- portable intrinsics, unsupported host extern handles, runtime context, and target imports.

Primitive nodes distinguish integer and floating arithmetic. Structural equality and numeric/Boolean operations are resolved before HIR so consumers never dispatch an operator by source spelling.

Function application is unary and curried. A multi-argument source function is represented as nested lambdas and applications.

## Patterns and matches

HIR retains FScript patterns before decision-tree lowering:

- wildcard and binding patterns;
- literal patterns;
- empty/cons list patterns;
- tuple and record patterns;
- ordered map clauses and optional remainder patterns;
- option and declared-union patterns;
- runtime type-reference patterns.

A match case contains a pattern, optional Boolean guard, body, and case span. Cases and guards retain source order. Pattern bindings use the same deterministic IDs as references in guards and bodies.

## Evaluation semantics

HIR child order is semantic:

- declarations and top-level expressions execute in source order;
- expression operands and aggregate elements evaluate left to right;
- failures stop subsequent evaluation;
- `&&` and `||` short-circuit their right operand;
- a match scrutinee is evaluated once;
- cases and guards are attempted in source order;
- a non-recursive binding initializer does not see its new binding;
- a recursive group sees every member of that group.

## Linkage

Compiler linkage is declarative and contains no executable .NET delegate:

- `Intrinsic name` identifies a supported portable builtin;
- `RuntimeContext` identifies `Env`;
- `HostExternal(name, arity)` identifies a host-only extern;
- `TargetImport(modulePath, memberName)` identifies backend-provided code.

Unsupported linkage remains representable in HIR so reachability-sensitive consumers can reject only executed or generated uses.

## Validation

`HirValidation.validate` returns `Result<Hir.Program, CompilerDiagnostic list>` and enforces at least:

- unique type and value symbol IDs;
- valid spans, including nested expressions and patterns;
- known named types and union cases;
- resolved references and pattern bindings;
- expression, branch, call, aggregate, and declaration type consistency;
- valid recursive groups and declaration schemes;
- valid primitive signatures;
- valid record conversion evidence;
- complete import and export metadata.

HIR diagnostics use `FSH1xxx` codes and preserve an originating FScript span whenever one exists.

## Deterministic debugging

`Hir.format : Hir.Program -> string` produces a canonical, human-readable representation with stable ordering, symbol IDs, and normalized types. It is intended for diagnostics and snapshot tests, not serialization.

The compiler serializes validated LIR, not formatted HIR.

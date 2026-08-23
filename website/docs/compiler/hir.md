---
id: compiler-hir
title: Typed HIR Specification
slug: /compiler/hir
---

The high-level IR is the first compiler-owned form after parsing, name resolution, and Hindley–Milner inference.

```text
Program
  -> inferResolvedProgramWithExterns
  -> HirLowering.lower
  -> HirValidation.validate
  -> Hir.Program
```

## Contract

HIR is fully typed and resolved, independent of parser AST nodes and backend representations, identified by deterministic program-local symbol IDs, and validated before LIR lowering.

Every expression and pattern carries its final FScript type and source span. Top-level declarations retain normalized polymorphic schemes, attributes, recursion metadata, and structured export metadata.

## Program contents

`Hir.Program` contains ordered type declarations, imported-record declarations, value declarations, and an optional final entry expression. Includes have already been resolved, so the program represents the complete compilation unit.

HIR retains lambdas and applications, branches and lexical bindings, records and unions, lists/options/maps, interpolation, ranges, loops, matches, and type tokens.

Operators are semantic operations such as integer addition, floating division, structural equality, or Boolean conjunction. Explicit `RecordConvert` nodes carry frontend-proven structural conversion evidence.

## Patterns

HIR retains wildcard, binding, literal, list, tuple, record, map, option, union, and type patterns. Match cases preserve guard and case order. Pattern bindings use the same IDs as references in guards and bodies.

Pattern decision plans do not appear until HIR is lowered to LIR.

## Evaluation requirements

- Top-level declarations execute in source order.
- Children and arguments evaluate left to right.
- `&&` and `||` short-circuit.
- Match scrutinees evaluate once.
- Cases and guards run in source order.
- Recursive groups see all peers; ordinary initializers see only preceding scope.

## Linkage and validation

HIR distinguishes portable intrinsics, the `Env` runtime context, target imports, and host-only extern handles. Linkage descriptions carry types and arity but no executable .NET delegates.

Validation checks unique IDs, known types, resolved references, valid spans, expression and scheme consistency, primitive signatures, recursive groups, conversion evidence, and import/export metadata. Failures use `FSH1xxx` diagnostics.

`Hir.format` produces a deterministic debug form for snapshots. It is not an artifact format; `.fsir` stores validated LIR.

Read the repository's [complete normative HIR specification](https://github.com/MagnusOpera/FScript/blob/main/docs/specs/typed-hir.md).

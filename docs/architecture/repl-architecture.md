# FScript REPL Architecture

## Purpose
This document describes how interactive REPL mode works in the `FScript` CLI host.

## Entry selection

The CLI chooses mode in `src/FScript/Program.fs`:

1. If a script path is provided, run file mode.
2. If stdin is redirected, run stdin mode.
3. Otherwise, run REPL mode.

This keeps pipelines (`cat file | fscript`) deterministic and prevents accidental REPL startup in redirected environments.

## Core loop

REPL mode is implemented in `runRepl` with these mutable state values:

- `baseProgram`: retained top-level declarations for session state.
- `pendingLines`: current buffered input.
- `pendingBlankLines`: blank-line counter while pending.
- `running`: loop flag.

Prompt behavior:

- `> ` when `pendingLines` is empty.
- `. ` when collecting a multiline input.

## Execution pipeline

When input is submitted, REPL runs the same language pipeline as non-interactive execution:

1. Parse pending source into AST statements.
2. Append parsed statements to retained `baseProgram`.
3. Type-infer combined program with runtime externs.
4. Evaluate combined program and read `LastValue`.
5. Print result for expression submissions.

This reuses the same parser, type inference, and evaluator behavior as script-file execution.

## Retention model

After successful execution:

- Non-expression statements from current input are appended to `baseProgram`.
- Expression statements are not retained.

Result:

- Definitions (`let`, types) persist across turns.
- Standalone expressions do not accumulate as top-level program entries.

## Multiline semantics

REPL attempts execution after each entered line:

- If parse is incomplete, it keeps collecting lines.
- While there is pending input, double-Enter triggers forced submission (second blank line).
- On EOF (`Ctrl+D`) with pending input, REPL tries one final submission before exit.

## Output semantics

- Non-function results are printed via `Pretty.valueToString`.
- Function results are formatted as typed signatures by combining inferred type info with closure parameter names when available.

This improves usability versus printing raw closure internals.

## Error handling

REPL catches and reports language-layer exceptions:

- `ParseException`
- `TypeException`
- `EvalException`

Behavior:

- Incomplete parse during line-by-line typing is treated as "still pending".
- Forced submissions that still fail parse print a parse error.
- Type/eval errors print diagnostics and reset pending input.

## Signals and termination

- `Ctrl+C`: cancels and exits process (`Environment.Exit(0)`).
- `Ctrl+D`: exits REPL loop (after trying to run pending input, if any).

## Runtime context

REPL builds `HostContext` with the resolved root directory (`-r/--root` or default current directory) and loads externs from `Registry.all`, matching normal CLI execution behavior.

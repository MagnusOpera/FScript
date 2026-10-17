# FScript REPL Guide

The FScript CLI starts an interactive REPL when you run `fscript` with no script path and no redirected stdin.

## Start

```bash
fscript
```

If you are running from source:

```bash
dotnet run --project src/FScript --
```

## Prompts

- `> ` means a new top-level input.
- `. ` means the REPL is waiting for more lines for the current input.

## Input and execution model

- The REPL tries to parse/evaluate after each entered line.
- If the input is syntactically incomplete, it keeps buffering lines.
- For a pending multiline block, enter two blank lines to force submission. Complete input may execute earlier because the REPL tries each line as it arrives.
- On successful execution, expression results are printed.
- If the result is a function value, the REPL prints a typed signature (for example `(x: int) -> int`) instead of an opaque closure value.

## Session state

- Top-level definitions (`let`, type declarations, etc.) are retained and available in later inputs.
- Pure expression entries are evaluated and printed, but are not retained as top-level declarations unless bound with `let`.

## Errors

- Type and evaluation errors are reported immediately and the pending buffer is reset.
- Parse errors for incomplete input are deferred while you are still typing. If you force submission (double-Enter on pending input), parse errors are reported.

## Exit

- `Ctrl+D` (EOF) exits the REPL. If there is pending input, the REPL attempts to run it first.
- `Ctrl+C` exits immediately.

## Root directory and imports

Set the sandbox/import root with `-r`/`--root`:

```bash
fscript -r /path/to/project
```

This sets the runtime filesystem root. Import loading requires an include-capable host/loader; the CLI REPL currently parses submissions without resolving file imports. Use file mode for multi-file scripts.

## Mode selection notes

- `fscript <script.fss>` runs file mode.
- `cat script.fss | fscript` runs stdin mode.
- `fscript` (no file, no redirected stdin) runs REPL mode.

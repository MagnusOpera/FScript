# BASIC Sample Interpreter

This folder contains a small, vintage-inspired BASIC interpreter implemented in FScript.

Run it with:

```bash
dotnet run --project src/FScript -- samples/basic/basic.fss -- helloworld.bas
```

Or with the installed CLI:

```bash
fscript samples/basic/basic.fss -- helloworld.bas
```

The first script argument after `--` is the BASIC program path. BASIC files are read relative to the sample root when you run `samples/basic/basic.fss`.

## Supported Program Format

- Line-numbered BASIC source only.
- One statement per line.
- Execution starts at the lowest line number.
- Blank lines are ignored.

Example:

```basic
10 PRINT "HELLO WORLD"
20 END
```

## Variables and Values

- Numeric variables hold integers only.
- String variables must end with `$`.
- Uninitialized numeric variables default to `0`.
- Uninitialized string variables default to `""`.

Examples:

```basic
10 LET COUNT = 10
20 NAME$ = "ADA"
30 PRINT COUNT
40 PRINT NAME$
```

## Statements

### `REM`

Comment line. Everything after `REM` is ignored.

```basic
10 REM this is a comment
```

### `LET`

Assigns a value to a variable.

```basic
10 LET A = 1
20 LET TITLE$ = "HELLO"
```

### Bare Assignment

`LET` is optional for assignments.

```basic
10 A = 1
20 TITLE$ = "HELLO"
```

### `PRINT`

Evaluates an expression and prints one line.

```basic
10 PRINT "HELLO"
20 PRINT 1 + 2
```

`PRINT` is implemented through `Console.writeLine` in the hosting FScript script.

### `INPUT`

Reads one line from standard input and stores it in a variable.

```basic
10 INPUT NAME$
20 INPUT COUNT
```

Behavior:

- `INPUT NAME$` stores the raw line as a string.
- `INPUT COUNT` parses the line as an integer.
- End-of-input is an error.
- Invalid integer input for a numeric variable is an error.

`INPUT` is implemented through `Console.readLine ()` in the hosting FScript script.

### `IF ... THEN <line>`

Evaluates a numeric condition and jumps when it is non-zero.

```basic
10 LET X = 3
20 IF X > 0 THEN 100
30 PRINT "NO JUMP"
40 END
100 PRINT "JUMPED"
110 END
```

### `GOTO`

Unconditional jump to a line number.

```basic
10 GOTO 100
100 PRINT "HERE"
110 END
```

### `GOSUB` / `RETURN`

Calls a subroutine and returns to the next line after `GOSUB`.

```basic
10 GOSUB 100
20 PRINT "BACK"
30 END
100 PRINT "SUBROUTINE"
110 RETURN
```

### `FOR ... TO ...` / `NEXT`

Simple increasing loops only.

```basic
10 FOR I = 1 TO 3
20 PRINT I
30 NEXT I
40 END
```

Behavior:

- The loop variable is set to the start value before the first iteration.
- If `start > end`, the loop body is skipped.
- `NEXT` must match the active `FOR` variable name.

### `END`

Stops program execution.

```basic
10 PRINT "DONE"
20 END
```

## Expressions

Supported expression forms:

- integer literals: `1`, `42`, `-7`
- string literals: `"HELLO"`
- variable references: `A`, `NAME$`
- parentheses: `(A + 1)`
- unary minus: `-A`

Supported arithmetic operators:

- `+`
- `-`
- `*`
- `/`

Supported comparisons:

- `=`
- `<>`
- `<`
- `<=`
- `>`
- `>=`

Notes:

- Arithmetic is integer-only.
- Division is integer division.
- `+` supports either `number + number` or `string + string`.
- String comparisons support only `=` and `<>`.
- Conditions are numeric: `0` is false, any non-zero value is true.

## Available Language Surface

This sample intentionally implements only a small subset. There are no extra BASIC built-in functions yet.

Available capabilities are:

- integer arithmetic
- string literals and string concatenation with `+`
- control flow through `IF`, `GOTO`, `GOSUB`, `RETURN`, `FOR`, `NEXT`, and `END`
- output through `PRINT`
- input through `INPUT`

## Errors

The interpreter reports runtime or parse errors for cases such as:

- duplicate line numbers
- missing jump targets
- `RETURN` without `GOSUB`
- `NEXT` without a matching `FOR`
- type mismatches between numeric and string variables
- division by zero
- malformed statements or expressions

## Not Implemented

The following are intentionally out of scope for this sample:

- arrays
- `DATA` / `READ`
- multiple statements on one line with `:`
- floating-point values
- logical operators like `AND`, `OR`, `NOT`
- machine-specific commands such as graphics, sound, `PEEK`, or `POKE`

## Bundled Examples

- [helloworld.bas](/Users/pct/src/MagnusOpera/FScript/samples/basic/helloworld.bas)
- [controlflow.bas](/Users/pct/src/MagnusOpera/FScript/samples/basic/controlflow.bas)
- [input.bas](/Users/pct/src/MagnusOpera/FScript/samples/basic/input.bas)

# Project tests

Run `fscript test` from the project directory. The CLI recursively discovers files ending in `_test.fss`. It skips symbolic links and runs files in ordinal path order. Tests can live beside the code they import.

```fsharp
import "pricing.fss" as Pricing

Test.case "zero strike" (fun () ->
    Assert.equal (Some 100.0) (Pricing.black Pricing.call 0.0 100.0 0.2 1.0 0.0))
```

The CLI evaluates each file once to register its cases. `Test.case : string -> (unit -> unit) -> unit` takes a nonempty name and a function that runs the test. Cases run sequentially in registration order and can share values initialized by that file. Registration is only available during file initialization.

`Test.each : string -> (string * 'a) list -> ('a -> unit) -> unit` registers one case per row. The first argument names the group. Each row provides a name and the input passed to the shared function. The group and row names must contain a non-whitespace character. Rows run in list order. An empty list contributes zero cases.

```fsharp
Test.each "doubling" [("zero", (0, 0)); ("positive", (3, 6))] (fun row ->
    match row with
    | (input, expected) -> Assert.equal expected (2 * input))
```

The runner uses the full name `group / row` for filtering and reporting. All case names must be unique within a file, including collisions between `Test.case` names and combined row names. The runner prints the row input on failure.

`Assert.equal : 'a -> 'a -> unit` uses FScript equality and reports the expected and actual values on failure. `Assert.near : float -> float -> float -> unit` takes an absolute tolerance, an expected value, and an actual value. The tolerance must be finite and nonnegative. A comparison passes when the absolute error is finite and at most the tolerance. A failed comparison reports the expected value, actual value, absolute error, and tolerance.

`Assert.relative : float -> float -> float -> unit` takes a tolerance, an expected value, and an actual value. Both values must be finite, and the tolerance must be finite and nonnegative. For a nonzero expected value, the assertion checks `abs(actual - expected) / abs(expected) <= tolerance`. For an expected value of zero, the actual value must equal zero. A failed comparison reports the values and, for finite values with a nonzero reference, the relative error and tolerance.

`Assert.some : 'a option -> 'a` returns the payload of `Some` and fails on `None`. `Assert.none : 'a option -> unit` accepts `None` and shows the actual value if given `Some`. Use `Assert.some` when the next assertion needs the payload, as in `Assert.near tolerance expected (Assert.some actual)`.

An assertion failure ends the current case. The runner reports the file, line, and column where the assertion was applied, including calls inside imported helpers, then continues with the next case. An error during initialization, including a file that registers no cases, fails that file and the runner continues with the next file. A nonzero `Env.setExitCode` also fails the case or initialization that set it. The command returns exit status 1 after failures or when no cases match, and 0 when all selected cases pass.

Use `--filter <substring>` for a case-sensitive substring match on case names. Filtering applies after initialization. The command reads `fscript.toml` from the project directory by default and accepts `--config`, `--no-config`, `--no-default-externs`, `--permissions`, and `--extern-assembly`. Defaults and permission checks follow ordinary script execution. The test API is available in the test command even with default externs disabled.


## Timing a test run

Add `--profile` to report wall-clock timings after the test summary. File setup covers prelude and import loading, parsing, type checking, and case registration. The report lists every file setup, including unsuccessful ones. It also shows the ten slowest selected cases in descending order, with each `Test.each` row timed separately. Failed cases are included. The totals cover all measured setups and selected cases, even when the case list shows only the slowest ten.

Use `--filter` with the case name to rerun a slow case. File setup still runs for each discovered file when a filter is active. Case time covers interpreter invocation and excludes the runner's own result printing. CLI startup and project configuration happen before these measurements.

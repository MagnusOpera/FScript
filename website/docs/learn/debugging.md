---
title: Debugging
---

Use the FScript VS Code extension and a recent CLI (`fscript` on PATH, or
`fscript.cli.path` pointing to an executable or `fscript.dll`) to debug a
saved `.fss` file. Debugging requires a trusted workspace and .NET execution;
it is unavailable in the JavaScript/Fable host.

## Start a session

Open a script and press **F5**. Choose **FScript** if VS Code asks for a debugger.
A minimal `.vscode/launch.json` configuration is:

```json
{
  "version": "0.2.0",
  "configurations": [
    {
      "type": "fscript",
      "request": "launch",
      "name": "Debug FScript",
      "program": "${file}",
      "stopOnEntry": false
    }
  ]
}
```

Use `args` for script arguments and `cwd` to override the terminal's working
directory. For example, `"args": ["--headless"]` runs the graphics sample's
headless mode. Input and output use VS Code's integrated terminal, so
`Console.readLine ()` continues to work.

The debugger uses the configuration selected by **FScript: Select Configuration**
for the program's workspace folder. Select the graphics sample's manifest before
debugging its `main.fss`. When no configuration is selected, debugging uses the
editor's default environment rather than discovering another manifest.

Try `samples/debugging/main.fss` in this repository. Set a breakpoint on
`adjusted * 2` in `worker.fss` to inspect parameters, locals, captures, and two
independent tasks. The pipeline in `run` also demonstrates expression stepping.

## Breakpoints and stepping

Click the editor gutter to set a breakpoint. Inline breakpoints distinguish
expressions on the same line. Nonexecutable locations remain unverified.

- **Step Into** enters FScript functions and callbacks.
- **Step Over** runs nested calls but still honors their breakpoints.
- **Step Out** returns to the caller.
- **Pause** stops at the next FScript expression boundary.

Stepping follows expressions, not just lines. Successive pipeline stages can
stop on the same source line. At call and pipeline return boundaries, Locals
contains `$result` with the completed operation's value.

The debugger can enter a FScript callback invoked by an extension, but cannot
step through the extension's .NET implementation. Pausing a graphics callback
also pauses its native caller, so its window may stop responding until you resume.

## Inspect values safely

Call Stack shows FScript functions and logical tasks. Variables exposes
parameters, locals, captures, and expandable collections. Host-managed values
show their visible fields; hidden state is inaccessible.

Watch and Debug Console expressions support names, imported qualified names,
visible field access, parentheses, and primitive literals:

```fsharp
camera
camera.Position
camera.Position.X
```

Calls such as `Console.writeLine "hello"` or `NDArray.set array [0] 1.0` are
rejected in watches. Inspection does not execute functions or mutate state.
Variable editing is unavailable.

## Tasks and session lifetime

A breakpoint pauses only the task that reaches it. Other tasks continue, and a
task awaiting the paused task waits naturally. Use Call Stack to select a task.
DAP single-task execution controls leave other paused tasks untouched; ordinary
Continue can resume them together.

Uncaught exceptions stop with the originating FScript stack. Resume to propagate
the failure using normal FScript task semantics.

Stop ends the launched program and releases paused tasks. An unresponsive native
call may require forced process termination. Restart debugging after changing
source, configuration, or extension assemblies. Attaching to an existing host is
not supported.

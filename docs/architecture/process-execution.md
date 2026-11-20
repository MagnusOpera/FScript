# Process execution

Runtime registers `Process.defaults`, `Process.exec`, `ProcessOptions`, and
`ProcessResult` in its default catalog. No assembly or NuGet provider import is
required. Disabling default externs removes this module along with other Runtime
externs. Options and result types and immutable defaults need no permissions;
execution requires `Native`. The descriptor also checks Native at invocation,
including when renamed by an embedding host. Child processes have host OS access;
the script filesystem sandbox does not contain them.

Process helpers remain under `FScript.Runtime/Processes`. `ProcessExterns` adapts
internal SDK descriptors through `ExtensionBridge`, using SDK 3
`ExternalCallContext.ExposeAsync` to register cancellable asynchronous I/O. The
factory starts inline and returns a platform task; it does not queue a worker or
block awaiting completion. Script task operations retain `Threading` checks.
Evaluation scopes join exposed I/O and pass cancellation to the process runner,
which terminates children and drains or closes their pipes during cleanup.

The compiler binds Runtime's typed `CompiledProcess` CLR companions through the
ordinary compiled-provider registry. No process-specific compiler intrinsic is
needed. Both compiled targets support Process; JavaScript and standalone LIR do
not provide the Runtime catalog.

## Local validation

Run `make sdk-test` from the FScript checkout to test the sibling Extensibility
SDK, all solution tests, and sample smoke tests together. Before SDK 3 is
published, set `UseLocalFScriptExtensibility=true` for normal dotnet or make
commands to reference the sibling SDK project instead of its NuGet package.
External libraries still depend only on Extensibility, and expose async I/O
without receiving script scheduling or awaiting APIs.

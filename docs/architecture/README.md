# FScript Architecture

How the repository and assemblies are organized.

- Assemblies and roles: [`assemblies-and-roles.md`](./assemblies-and-roles.md)
- REPL architecture: [`repl-architecture.md`](./repl-architecture.md)
- Typed IR and backends: [`typed-ir-and-backends.md`](./typed-ir-and-backends.md)
- Native AOT size investigation: [`native-aot-size.md`](./native-aot-size.md)

Repository validation uses a Terrabuild graph with a Docker toolchain phase followed by containerized application tasks: one solution build supplies cached managed outputs to nine independent .NET test tasks and the debugger adapter tests. Extension, Fable, and website checks can run alongside them. The SDK compatibility check follows the graph and uses temporary intermediate artifacts to preserve cached solution outputs. See [Building FScript from source](../guides/building.md) for commands and Insights configuration.

Make delegates common build and validation targets to this graph. The container task scripts call tools directly to keep the dependency direction Make → Terrabuild → tools; interactive, benchmark, and packaging commands remain direct.

The solution build stores shared assembly and intermediate outputs in a project-local archive. A lazy `materialize` task restores that archive before consumers execute; generated outputs outside the orchestration project are excluded from input hashing.

Tool versions are declared in `WORKSPACE`. The shared GitHub `setup-tools` action installs native Terrabuild, while Docker build arguments supply application tool versions. Ordinary validation and website deployment require no host SDK; legacy release and native macOS verification jobs explicitly opt into host tools through the same action.

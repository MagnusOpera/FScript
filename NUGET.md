![FScript logo](https://raw.githubusercontent.com/FScriptLang/FScript/refs/heads/main/FScript.png)

# FScript

FScript is a lightweight, embeddable interpreter with an F#/ML-style language.

## Why

- F#/ML-style scripting with immutable-by-design, expression-oriented semantics.
- Strong static checks with Hindley-Milner type inference.
- Host-controlled extensibility through typed external functions.
- Sandbox-aware runtime model for controlled filesystem and host access.

## Extension authoring

Reference the independently versioned `MagnusOpera.FScript.Extensibility` SDK to author providers without interpreter dependencies. Embedding applications use Language/Runtime and adapt SDK registrations through `ExtensionBridge.toLanguageRegistration`. Runtime requires SDK 3.x with unsigned assembly identity 3.0.0.0. SDK 2 hosts/providers must rebuild for the new async-only invocation context.

## Process functions

Runtime supplies `Process.exec`, gated by `Native`, and permission-free
`Process.defaults` and data types. No external package import is required.
Process execution uses SDK 3 async I/O; script task operations such as
`Task.await` separately require `Threading`.
See [process execution and local testing](docs/architecture/process-execution.md).

## License

This project is licensed under the MIT License. See `LICENSE`.

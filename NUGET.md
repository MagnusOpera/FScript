![FScript logo](https://raw.githubusercontent.com/MagnusOpera/FScript/refs/heads/main/FScript.png)

# FScript

FScript is a lightweight, embeddable interpreter with an F#/ML-style language.

## Why

- F#/ML-style scripting with immutable-by-design, expression-oriented semantics.
- Strong static checks with Hindley-Milner type inference.
- Host-controlled extensibility through typed external functions.
- Sandbox-aware runtime model for controlled filesystem and host access.

## Extension authoring

Reference the independently versioned `MagnusOpera.FScript.Extensibility` SDK to author providers without interpreter dependencies. Embedding applications use Language/Runtime and adapt SDK registrations through `ExtensionBridge.toLanguageRegistration`. Compatible SDK 1.x releases retain unsigned assembly identity 1.0.0.0 independently of FScript product versions.

## License

This project is licensed under the MIT License. See `LICENSE`.

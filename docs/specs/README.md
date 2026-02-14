# FScript Specifications

Normative behavior for the language, runtime surface, hosting model, and editor/LSP semantics.

## Language and type system

- Syntax and indentation: [`syntax-and-indentation.md`](./syntax-and-indentation.md)
- Supported types: [`supported-types.md`](./supported-types.md)
- Function annotations: [`function-annotations.md`](./function-annotations.md)
- Map matching: [`map-matching-reference.md`](./map-matching-reference.md)

## Standard library and extern model

- Stdlib functions: [`stdlib-functions.md`](./stdlib-functions.md)
- External functions and extensibility: [`external-functions.md`](./external-functions.md)

## Hosting and security

- Embedding `FScript.Language`: [`embedding-fscript-language.md`](./embedding-fscript-language.md)
- Sandbox and security: [`sandbox-and-security.md`](./sandbox-and-security.md)

## Editor/LSP behavior

- LSP inlay hints: [`lsp-inlay-hints.md`](./lsp-inlay-hints.md)
- LSP uses runtime extern schemes for typing/signatures and resolves navigation to included-file declarations.
- LSP injected stdlib functions show named-argument signatures when available and resolve definition to readonly virtual stdlib sources (`fscript-stdlib:///...`).
- Definition/type-definition from record field labels in function return record literals resolves to the declared return type (including include-provided types).

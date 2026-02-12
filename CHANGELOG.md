# Changelog

All notable changes to FScript are documented in this file.

## [Unreleased]

### Documentation
- Added dedicated map matching reference: `docs/map-matching-reference.md`.
- Added include-resolution details (path normalization, root confinement, include deduplication, file-aware errors) in `docs/syntax-and-indentation.md`.
- Added embedding cookbook section to `docs/embedding-fscript-language.md`.
- Added and expanded onboarding tutorial and README navigation polish.

## [0.23.1]

### Documentation
- Refined onboarding tutorial and prioritized tutorial-first navigation in `README.md`.

## [0.23.0]

### Documentation
- Added progressive getting-started tutorial and linked it from docs/README.

## [0.22.0]

### Documentation
- Updated docs and samples for `int` + `string` map keys.

## [0.21.0]

### Language
- Added `int` and `string` map key support with inferred polymorphic empty maps.

## [0.20.0]

### Language
- Improved map pattern matching semantics.

## [0.19.0]

### Language
- Added `when` guards in match cases.

## [0.18.0]

### Language
- Added map spread syntax (`{ [k] = v; ..tail }`).
- Restricted append operator `@` to list-only usage.

## [0.17.0]

### Language and stdlib
- Added map pattern matching.
- Moved collection helpers to stdlib-first model.

## [0.16.0]

### Language
- Added qualified discriminated union constructors/pattern support.

## [0.15.2]

### Packaging
- Stopped packing `FScript.png` in NuGet packages.

## [0.15.1]

### Documentation
- Updated README content.

## [0.15.0]

### Packaging
- Added NuGet package icon and branding assets.

## [0.14.0]

### Language
- Improved indentation diagnostics for misindented expressions.

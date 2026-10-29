---
id: stdlib-encoding
title: Encoding Module
slug: /stdlib/encoding
---

These functions are pure .NET runtime externs. Hosts may register `PureExterns.functions` without exposing IO; the CLI registers them by default. `--no-default-externs` removes them. JavaScript hosts must supply corresponding externs themselves.

Bytes are represented as `int list`; every element must be in `0..255`. Functions return `Some result` on success and `None` for invalid content. Inputs are never read from files, and no system encoding or current culture is consulted.

### `Encoding.utf8Encode`

```fsharp
Encoding.utf8Encode : string -> int list option
```

Encodes text as UTF-8 bytes without a BOM. Unpaired UTF-16 surrogates return `None`.

```fsharp
Encoding.utf8Encode "é"
// Some [195;169]
```

### `Encoding.utf8Decode`

```fsharp
Encoding.utf8Decode : int list -> string option
```

Strictly decodes UTF-8. Invalid byte values or malformed UTF-8 return `None`. A supplied BOM is preserved as U+FEFF.

```fsharp
Encoding.utf8Decode [195;169]
// Some "é"
```

### `Encoding.base64Encode`

```fsharp
Encoding.base64Encode : int list -> string option
```

Encodes bytes with the standard Base64 alphabet and padding, without line breaks.

```fsharp
Encoding.base64Encode [97;98;99]
// Some "YWJj"
```

### `Encoding.base64Decode`

```fsharp
Encoding.base64Decode : string -> int list option
```

Decodes standard padded Base64; .NET Base64 whitespace is accepted. Invalid input returns `None`.

```fsharp
Encoding.base64Decode "YWJj"
// Some [97;98;99]
```

### `Encoding.hexEncode`

```fsharp
Encoding.hexEncode : int list -> string option
```

Encodes bytes as lowercase hexadecimal, with two digits per byte.

```fsharp
Encoding.hexEncode [0;171;255]
// Some "00abff"
```

### `Encoding.hexDecode`

```fsharp
Encoding.hexDecode : string -> int list option
```

Decodes even-length hexadecimal; either case is accepted. Whitespace, odd lengths, and invalid digits return `None`.

```fsharp
Encoding.hexDecode "00AbFF"
// Some [0;171;255]
```

### `Encoding.uriEncode`

```fsharp
Encoding.uriEncode : string -> string option
```

Percent-encodes a URI component using UTF-8, leaving only RFC 3986 unreserved characters unchanged. Invalid UTF-16 returns `None`.

```fsharp
Encoding.uriEncode "a +/é"
// Some "a%20%2B%2F%C3%A9"
```

### `Encoding.uriDecode`

```fsharp
Encoding.uriDecode : string -> string option
```

Decodes percent escapes and strictly validates UTF-8. Invalid escapes or UTF-8 return `None`. A plus sign remains a plus sign; this is not form decoding.

```fsharp
Encoding.uriDecode "a%20%2B%2F%C3%A9"
// Some "a +/é"
```

## Register only pure content helpers

A .NET embedding host can expose all Encoding functions and the new SHA-256 functions without constructing an IO-capable registry:

```fsharp
open FScript.Language
open FScript.Runtime

let registration = HostInterop.ofExterns PureExterns.functions
let result =
    Parser.parseProgram "Hash.sha256 \"hello\""
    |> TypeInfer.inferProgramWithRegistration registration
    |> Eval.evalProgramWithRegistration registration
```

Core scalar math, String, List, Map, Option, Path, Glob, and SemVer helpers need no registration. The scalar math functions live in `Int` and `Float`.

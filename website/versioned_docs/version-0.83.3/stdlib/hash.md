---
id: stdlib-hash
title: Hash Module
slug: /stdlib/hash
---

`Hash` is provided by the default .NET runtime extern registry.

## `Hash.md5 : string -> string option`

Hashes the UTF-8 bytes of the input and returns a lowercase, 32-character hexadecimal digest. Returns `None` if hashing fails.

```fsharp
let digest = Hash.md5 "hello"
// Some "5d41402abc4b2a76b9719d911017c592"
```

Use this for compatibility checks or non-security fingerprints. MD5 is unsuitable for password storage or security-sensitive integrity checks.

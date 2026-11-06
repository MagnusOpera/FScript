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

## Pure support additions

### `Hash.sha256`

```fsharp
Hash.sha256 : string -> string option
```

Returns a lowercase SHA-256 digest of the supplied text’s strict UTF-8 bytes, without a BOM. Invalid UTF-16 returns `None`.

```fsharp
Hash.sha256 "abc"
// Some "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
```

### `Hash.sha256Bytes`

```fsharp
Hash.sha256Bytes : int list -> string option
```

Returns a lowercase SHA-256 digest of supplied bytes. Invalid byte values return `None`.

```fsharp
Hash.sha256Bytes [97;98;99]
// Some "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
```

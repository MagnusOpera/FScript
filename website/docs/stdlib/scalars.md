---
id: stdlib-scalars
title: Int, Float, Bool Modules
slug: /stdlib/scalars
---

## Int

### `Int.tryParse : string -> int option`

Parses an invariant integer string.

```fsharp
let maybeN = Int.tryParse "42"
```

### `Int.toString : int -> string`

Formats an integer using invariant formatting.

```fsharp
let text = Int.toString 42
```

## Float

### `Float.tryParse : string -> float option`

Parses an invariant floating-point string.

```fsharp
let maybePi = Float.tryParse "3.14"
```

### `Float.toString : float -> string`

Formats a float using a round-trippable representation.

```fsharp
let text = Float.toString 3.14
```

## Bool

### `Bool.tryParse : string -> bool option`

Parses `true` or `false`.

```fsharp
let maybeFlag = Bool.tryParse "true"
```

### `Bool.toString : bool -> string`

Formats a boolean as lowercase text.

```fsharp
let text = Bool.toString true
```

## Parsing pattern

Use parsing helpers with options:

```fsharp
let port =
  Int.tryParse "8080"
  |> Option.defaultValue 80
```

## Pure support additions

Scalar math uses signed 64-bit integers and IEEE-754 doubles. Transcendental functions follow the host math library; last-bit results may differ across platforms, so they are not a portable bitwise cache-key format. Invalid floating math domains generally produce NaN/infinity as specified below.

### `Int.abs`

```fsharp
Int.abs : int -> int
```

Returns the absolute value. The minimum signed 64-bit integer raises an error because its positive value is out of range.

```fsharp
Int.abs (0 - 3)
// 3
```

### `Int.sign`

```fsharp
Int.sign : int -> int
```

Returns `-1`, `0`, or `1`.

```fsharp
Int.sign (0 - 3)
// -1
```

### `Int.min`

```fsharp
Int.min : int -> int -> int
```

Returns the smaller value.

```fsharp
Int.min 2 3
// 2
```

### `Int.max`

```fsharp
Int.max : int -> int -> int
```

Returns the larger value.

```fsharp
Int.max 2 3
// 3
```

### `Int.clamp`

```fsharp
Int.clamp : int -> int -> int -> int
```

Bounds the value inclusively. Reversed bounds raise an error.

```fsharp
Int.clamp 0 10 20
// 10
```

### `Float.abs`

```fsharp
Float.abs : float -> float
```

Returns the absolute value, propagating NaN.

```fsharp
Float.abs (0.0 - 3.0)
// 3
```

### `Float.sign`

```fsharp
Float.sign : float -> int
```

Returns `-1`, `0`, or `1`. NaN raises an error.

```fsharp
Float.sign (0.0 - 3.0)
// -1
```

### `Float.min`

```fsharp
Float.min : float -> float -> float
```

Returns the smaller value. Either NaN input produces NaN.

```fsharp
Float.min 2.0 3.0
// 2
```

### `Float.max`

```fsharp
Float.max : float -> float -> float
```

Returns the larger value. Either NaN input produces NaN.

```fsharp
Float.max 2.0 3.0
// 3
```

### `Float.clamp`

```fsharp
Float.clamp : float -> float -> float -> float
```

Bounds the value inclusively. Reversed or NaN bounds raise an error; a NaN value produces NaN.

```fsharp
Float.clamp 0.0 10.0 20.0
// 10
```

### `Float.floor`

```fsharp
Float.floor : float -> float
```

Rounds toward negative infinity; the result remains a float.

```fsharp
Float.floor 1.8
// 1
```

### `Float.ceil`

```fsharp
Float.ceil : float -> float
```

Rounds toward positive infinity; the result remains a float.

```fsharp
Float.ceil 1.2
// 2
```

### `Float.round`

```fsharp
Float.round : float -> float
```

Rounds to the nearest integer-valued float, with ties to even.

```fsharp
Float.round 2.5
// 2
```

### `Float.truncate`

```fsharp
Float.truncate : float -> float
```

Discards the fractional part toward zero; the result remains a float.

```fsharp
Float.truncate (0.0 - 1.8)
// -1
```

### `Float.sqrt`

```fsharp
Float.sqrt : float -> float
```

Returns the square root. Negative inputs produce NaN.

```fsharp
Float.sqrt 9.0
// 3
```

### `Float.log`

```fsharp
Float.log : float -> float
```

Returns the natural logarithm. Zero produces negative infinity; negative inputs produce NaN.

```fsharp
Float.log 1.0
// 0
```

### `Float.log10`

```fsharp
Float.log10 : float -> float
```

Returns the base-10 logarithm. Zero produces negative infinity; negative inputs produce NaN.

```fsharp
Float.log10 100.0
// 2
```

### `Float.exp`

```fsharp
Float.exp : float -> float
```

Returns the natural exponential. Overflow produces positive infinity.

```fsharp
Float.exp 0.0
// 1
```

### `Float.sin`

```fsharp
Float.sin : float -> float
```

Returns the sine of an angle in radians.

```fsharp
Float.sin 0.0
// 0
```

### `Float.cos`

```fsharp
Float.cos : float -> float
```

Returns the cosine of an angle in radians.

```fsharp
Float.cos 0.0
// 1
```

### `Float.tan`

```fsharp
Float.tan : float -> float
```

Returns the tangent of an angle in radians.

```fsharp
Float.tan 0.0
// 0
```

### `Float.asin`

```fsharp
Float.asin : float -> float
```

Returns the inverse sine in radians. Inputs outside `[-1, 1]` produce NaN.

```fsharp
Float.asin 0.0
// 0
```

### `Float.acos`

```fsharp
Float.acos : float -> float
```

Returns the inverse cosine in radians. Inputs outside `[-1, 1]` produce NaN.

```fsharp
Float.acos 1.0
// 0
```

### `Float.atan`

```fsharp
Float.atan : float -> float
```

Returns the inverse tangent in radians.

```fsharp
Float.atan 0.0
// 0
```

### `Float.pow`

```fsharp
Float.pow : float -> float -> float
```

Raises the first argument to the power of the second.

```fsharp
Float.pow 2.0 3.0
// 8
```

### `Float.atan2`

```fsharp
Float.atan2 : float -> float -> float
```

Returns the angle in radians for coordinates `(y, x)`: the first argument is y, the second x.

```fsharp
Float.atan2 0.0 1.0
// 0
```

### `Float.isNaN`

```fsharp
Float.isNaN : float -> bool
```

Tests for NaN.

```fsharp
Float.isNaN (Float.sqrt (0.0 - 1.0))
// true
```

### `Float.isInfinity`

```fsharp
Float.isInfinity : float -> bool
```

Tests for either positive or negative infinity.

```fsharp
Float.isInfinity (Float.exp 1000.0)
// true
```

### `Float.isFinite`

```fsharp
Float.isFinite : float -> bool
```

Tests that the value is neither NaN nor infinity.

```fsharp
Float.isFinite 1.0
// true
```

### `Float.pi`

```fsharp
Float.pi : float
```

The double-precision constant π. This is a value, not a function.

```fsharp
Float.pi > 3.0 && Float.pi < 4.0
// true
```

### `Int.toFloat`

```fsharp
Int.toFloat : int -> float
```

Converts an integer to a float. Values beyond the exact integer range of a double may lose precision.

```fsharp
Int.toFloat 3
// 3
```

### `Float.tryToInt`

```fsharp
Float.tryToInt : float -> int option
```

Truncates toward zero and returns `Some int`. NaN, infinity, or an input outside `[-2^63, 2^63)` returns `None`.

```fsharp
Float.tryToInt (0.0 - 2.9)
// Some -2
```

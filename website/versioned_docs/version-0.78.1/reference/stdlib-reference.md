---
id: stdlib-reference
title: Stdlib and Built-ins
slug: /reference/stdlib
---

This is the quick lookup page for the built-in FScript surface and the default runtime-provided stdlib helpers.

## Modules

- [`Console`](../stdlib/console)
- [`Task`](../stdlib/task)
- [`List`](../stdlib/list)
- [`Option`](../stdlib/option)
- [`Map`](../stdlib/map)
- [`String`](../stdlib/string)
- [`Int`, `Float`, `Bool`](../stdlib/scalars)

## Top-level built-ins

- [`Env`, `Environment`, `FsKind`, `ignore`](../stdlib/builtins)

## Console

- [`Console`](../stdlib/console)
- `Console.writeLine : string -> unit`
- `Console.readLine : unit -> string option`

## Native types

- [`task`, `list`, `map`, `option`, tuples, records, unions`](/manual/reference/native-types)

## Core built-in groups

- `Console.*`
- `List.*`
- `Option.*`
- `Map.*`
- `String.*`
- default runtime task helpers: `Task.spawn`, `Task.await`
- parsing helpers: `Int.tryParse`, `Float.tryParse`, `Bool.tryParse`
- scalar formatters: `Int.toString`, `Float.toString`, `Bool.toString`
- environment types/values: `Environment`, `FsKind`, `Env`
- top-level functions: `ignore`
- console I/O: `Console.writeLine`, `Console.readLine`

## Runtime extern families

- `Fs.*` filesystem utilities
- `Task.*`
- `Console.*`
- `Json.*`
- `Xml.*`
- `Regex.*`
- hashing and GUID helpers

Use the Stdlib pages for complete module-by-module function reference, and the native types page for language-level access forms such as `list[index]` and `map["key"]`.

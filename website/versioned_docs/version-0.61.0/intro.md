---
id: intro
title: Introduction
slug: /
---

FScript is a lightweight, embeddable programming language with an F#/ML-style syntax and a host-first runtime model.

This manual is intentionally language-first: it starts by teaching script writing, then moves into host integration and advanced reference material.

## What FScript is for

FScript is designed for applications that need:

- a compact scripting language for user or internal automation,
- static checks before execution (type inference),
- expression-oriented code with strong pattern matching,
- explicit control over capabilities exposed to scripts.

In practice, this makes FScript a good fit for embedded scripting in tools, automation platforms, build systems, and domain-specific workflows where safety and predictability matter.

## What this manual teaches

By the end of this manual, you should be comfortable with:

- writing and running `.fss` scripts,
- modeling data with records, unions, lists, maps, and options,
- using pattern matching for clear control flow,
- using the standard library effectively,
- exposing host functions (externs) when embedding FScript.

## Why FScript

- Expression-first language design.
- Static type inference (Hindley-Milner style).
- Strong pattern matching.
- Host-controlled capabilities through extern functions.

## Suggested learning path

1. Start with [Quickstart](./learn/quickstart) to install and run your first script.
2. Continue with [Language Fundamentals](./language/values-and-bindings) and work through Language chapters in order.
3. Use [Examples](./examples/guided-examples) to practice complete script patterns.
4. Study [Stdlib](./stdlib/overview) to learn the built-in modules in depth.
5. Move to [Embedding](./embedding/overview) when you need host integration.

## Manual structure

- **Learn**: onboarding and first working scripts.
- **Language**: core syntax, data modeling, typing, imports/exports, layout rules.
- **Examples**: practical scripts and reusable patterns.
- **Embedding**: host integration model, extern registration, safety boundaries.
- **Stdlib**: detailed function-by-function reference and recipes.
- **Reference**: CLI usage and quick lookup material.

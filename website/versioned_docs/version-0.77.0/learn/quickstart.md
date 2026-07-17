---
id: quickstart
title: Quickstart
slug: /learn/quickstart
---

# Your first useful FScript in five minutes

You will build a small pricing rule, run it, and see the language features that make FScript useful inside a host application.

:::tip Zero-install route
Want to touch the language before installing anything? Open the [browser playground](/sandbox), change a number in the first example, and press **Run**.
:::

## 1. Install the CLI

Choose one route. The .NET tool works anywhere supported by the .NET SDK; Homebrew is the shortest path on macOS.

### .NET tool

```bash
dotnet tool install --global MagnusOpera.FScript
```

### Homebrew

```bash
brew install magnusopera/tap/fscript
```

Check the installation:

```bash
fscript version
```

<details>
<summary>Building from source instead</summary>

```bash
git clone https://github.com/MagnusOpera/FScript.git
cd FScript
make build
```

Run scripts with `dotnet run --project src/FScript -- your-script.fss`.

</details>

## 2. Create a rule

Create `pricing.fss`:

```fsharp
type Plan =
  | Starter
  | Team
  | Scale

let pricePerSeat plan =
  match plan with
  | Starter -> 9
  | Team -> 19
  | Scale -> 29

let quote plan seats =
  pricePerSeat plan * seats

let total = quote Scale 12
Console.writeLine $"12 Scale seats: ${total}/month"
```

Run it:

```bash
fscript pricing.fss
```

You should see:

```text
12 Scale seats: $348/month
```

You have already used four important ideas:

- `type Plan` names the only valid plan choices.
- `match` handles every choice explicitly.
- `let quote plan seats` declares a function without type boilerplate.
- the compiler infers the types and checks the program before it runs.

Try removing the `Scale` branch or calling `quote "Scale" 12`. The type checker will stop the mistake at the script boundary.

## 3. Make the rule host-callable

An embedded script exposes only the bindings you mark as part of its host contract. Add the `export` annotation:

```fsharp
[<export>] let quote plan seats =
  pricePerSeat plan * seats
```

Your .NET or JavaScript host can now load the script and invoke `quote` by name. Everything else remains an implementation detail of the script.

- Follow the complete [.NET host example](../embedding/real-world-embedding).
- Use FScript from [JavaScript or a browser host](../embedding/fable-javascript).
- Learn how [typed externs](../embedding/register-externs) let scripts call capabilities supplied by your application.

## 4. Pick your next move

| If you want to… | Go here |
| --- | --- |
| Learn the core syntax quickly | [Language Tour](./language-tour) |
| Change and run examples | [Browser Playground](/sandbox) |
| Build something recognizable | [Guided Examples](../examples/guided-examples) |
| Put FScript inside an application | [Embedding Overview](../embedding/overview) |

## REPL and script arguments

Run `fscript` with no file to start an interactive session:

```bash
fscript
```

Pass script arguments after `--`:

```bash
fscript pricing.fss -- acme scale
```

The script receives them as the `string list` value `Env.Arguments`.

## Editor setup

The official VS Code extension adds diagnostics, completion, hover, go-to-definition, references, rename, semantic tokens, and inlay hints.

- [VS Code Marketplace](https://marketplace.visualstudio.com/items?itemName=MagnusOpera.fscript)
- [Open VSX](https://open-vsx.org/extension/MagnusOpera/fscript)

See [Editor Setup](./editor-setup) for the full walkthrough.

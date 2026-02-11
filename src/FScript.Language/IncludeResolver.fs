namespace FScript.Language

open System
open System.IO

module IncludeResolver =
    let private normalizeDirectoryPath (path: string) =
        let full = Path.GetFullPath(path)
        if full.EndsWith(Path.DirectorySeparatorChar.ToString(), StringComparison.Ordinal) then
            full
        else
            full + string Path.DirectorySeparatorChar

    let private ensureFssPath (path: string) (span: Span) =
        if not (path.EndsWith(".fss", StringComparison.OrdinalIgnoreCase)) then
            raise (ParseException { Message = "Only '.fss' files can be used with '#include'"; Span = span })

    let private ensureWithinRoot (rootDirectoryWithSeparator: string) (path: string) (span: Span) =
        let fullPath = Path.GetFullPath(path)
        let fullRoot = rootDirectoryWithSeparator.TrimEnd(Path.DirectorySeparatorChar)
        let isRootItself = String.Equals(fullPath, fullRoot, StringComparison.OrdinalIgnoreCase)
        let isUnderRoot = fullPath.StartsWith(rootDirectoryWithSeparator, StringComparison.OrdinalIgnoreCase)
        if not (isRootItself || isUnderRoot) then
            raise (ParseException { Message = $"Included file '{fullPath}' is outside of sandbox root"; Span = span })
        fullPath

    let private resolveIncludePath (currentFile: string) (includePath: string) (rootDirectoryWithSeparator: string) (span: Span) =
        if String.IsNullOrWhiteSpace(includePath) then
            raise (ParseException { Message = "Include path cannot be empty"; Span = span })

        ensureFssPath includePath span

        let currentDirectory = Path.GetDirectoryName(currentFile)
        let candidate =
            if Path.IsPathRooted(includePath) then includePath
            elif String.IsNullOrEmpty(currentDirectory) then includePath
            else Path.Combine(currentDirectory, includePath)

        ensureWithinRoot rootDirectoryWithSeparator candidate span

    let parseProgramFromFile (rootDirectory: string) (entryFile: string) : Program =
        let rootDirectoryWithSeparator = normalizeDirectoryPath rootDirectory
        let visited = System.Collections.Generic.HashSet<string>(StringComparer.OrdinalIgnoreCase)
        let fileSpan path =
            let p = Span.posInFile path 1 1
            Span.mk p p

        let rec loadFile (stack: string list) (filePath: string) : Program =
            let fullFilePath = Path.GetFullPath(filePath)
            let initialSpan = fileSpan fullFilePath
            ensureFssPath fullFilePath initialSpan
            let sandboxedPath = ensureWithinRoot rootDirectoryWithSeparator fullFilePath initialSpan

            if stack |> List.exists (fun p -> String.Equals(p, sandboxedPath, StringComparison.OrdinalIgnoreCase)) then
                let cycleChain = (sandboxedPath :: stack |> List.rev) @ [ sandboxedPath ]
                let message = sprintf "Include cycle detected: %s" (String.concat " -> " cycleChain)
                raise (ParseException { Message = message; Span = fileSpan sandboxedPath })

            if visited.Contains(sandboxedPath) then
                []
            else
                visited.Add(sandboxedPath) |> ignore
                let source = File.ReadAllText(sandboxedPath)
                let program = Parser.parseProgramWithSourceName (Some sandboxedPath) source
                expandProgram (sandboxedPath :: stack) sandboxedPath program

        and expandProgram (stack: string list) (currentFile: string) (program: Program) : Program =
            program
            |> List.collect (function
                | SInclude(includePath, span) ->
                    let resolvedPath = resolveIncludePath currentFile includePath rootDirectoryWithSeparator span
                    loadFile stack resolvedPath
                | stmt -> [ stmt ])

        loadFile [] entryFile

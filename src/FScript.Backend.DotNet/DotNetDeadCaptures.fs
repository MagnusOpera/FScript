namespace FScript.Compiler

open System.Collections.Generic
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax

/// Remove only compiler-created scaffolding whose evaluation is inert. Calls,
/// property reads, allocations and arithmetic are deliberately not classified here.
module internal DotNetDeadCaptures =
    let rec private inertSyntax (node: ExpressionSyntax) =
        match node with
        | :? IdentifierNameSyntax | :? LiteralExpressionSyntax | :? DefaultExpressionSyntax
        | :? LambdaExpressionSyntax -> true
        | :? CastExpressionSyntax as value ->
            let rec lambda (node: ExpressionSyntax) =
                match node with
                | :? LambdaExpressionSyntax -> true
                | :? ParenthesizedExpressionSyntax as inner -> lambda inner.Expression
                | _ -> false
            lambda value.Expression
        | :? ParenthesizedExpressionSyntax as value -> inertSyntax value.Expression
        | :? PostfixUnaryExpressionSyntax as value when value.IsKind SyntaxKind.SuppressNullableWarningExpression -> inertSyntax value.Operand
        | _ -> false

    type private Rewrite(model: SemanticModel, parameters: Dictionary<ISymbol,Set<int>>, dead: HashSet<ISymbol>) =
        inherit CSharpSyntaxRewriter()
        override this.VisitMethodDeclaration(node) =
            let rewritten = (base.VisitMethodDeclaration(node) |> nonNull) :?> MethodDeclarationSyntax
            let symbol = model.GetDeclaredSymbol(node) |> nonNull
            match parameters.TryGetValue(symbol) with
            | true, removed -> rewritten.WithParameterList(rewritten.ParameterList.WithParameters(SyntaxFactory.SeparatedList(rewritten.ParameterList.Parameters |> Seq.mapi (fun index item -> index,item) |> Seq.choose (fun (index,item) -> if removed.Contains index then None else Some item))))
            | _ -> rewritten
        override this.VisitInvocationExpression(node) =
            let rewritten = (base.VisitInvocationExpression(node) |> nonNull) :?> InvocationExpressionSyntax
            let symbol = model.GetSymbolInfo(node).Symbol
            if isNull symbol then rewritten else
                match parameters.TryGetValue(nonNull symbol) with
                | true, removed -> rewritten.WithArgumentList(rewritten.ArgumentList.WithArguments(SyntaxFactory.SeparatedList(rewritten.ArgumentList.Arguments |> Seq.mapi (fun index item -> index,item) |> Seq.choose (fun (index,item) -> if removed.Contains index then None else Some item))))
                | _ -> rewritten
        override this.VisitLocalDeclarationStatement(node) =
            if node.Declaration.Variables.Count=1 && dead.Contains(model.GetDeclaredSymbol(node.Declaration.Variables.[0]) |> nonNull) then
                SyntaxFactory.EmptyStatement() :> SyntaxNode
            else base.VisitLocalDeclarationStatement(node) |> nonNull
        override this.VisitExpressionStatement(node) =
            match node.Expression with
            | :? AssignmentExpressionSyntax as assignment when assignment.IsKind SyntaxKind.SimpleAssignmentExpression ->
                let symbol = model.GetSymbolInfo(assignment.Left).Symbol
                if not (isNull symbol) && dead.Contains(nonNull symbol) then SyntaxFactory.EmptyStatement() :> SyntaxNode
                else base.VisitExpressionStatement(node) |> nonNull
            | _ -> base.VisitExpressionStatement(node) |> nonNull

    let optimize className (input: CSharpCompilation) =
        let rec iterate (compilation: CSharpCompilation) =
            let parameters = Dictionary<ISymbol,Set<int>>(SymbolEqualityComparer.Default)
            let dead = HashSet<ISymbol>(SymbolEqualityComparer.Default)
            for tree in compilation.SyntaxTrees do
                let model = compilation.GetSemanticModel(tree,false)
                let root = tree.GetRoot()
                // Lifted IIFEs can be closure factories. Discard their allocation only
                // after proving their bodies and every supplied argument inert.
                let factories = HashSet<ISymbol>(SymbolEqualityComparer.Default)
                let rec inert (node: ExpressionSyntax) =
                    if inertSyntax node then true else
                        match node with
                        | :? InvocationExpressionSyntax as call ->
                            let symbol = model.GetSymbolInfo(call).Symbol
                            not (isNull symbol) && factories.Contains(nonNull symbol) && call.ArgumentList.Arguments |> Seq.forall (fun arg -> inert arg.Expression)
                        | _ -> false
                let rec inertStatement (node: StatementSyntax) =
                    match node with
                    | :? BlockSyntax as block -> block.Statements |> Seq.forall inertStatement
                    | :? EmptyStatementSyntax -> true
                    | :? ReturnStatementSyntax as statement -> not (isNull statement.Expression) && inert (nonNull statement.Expression)
                    | :? LocalDeclarationStatementSyntax as statement -> statement.Declaration.Variables |> Seq.forall (fun variable -> not (isNull variable.Initializer) && inert (nonNull variable.Initializer).Value)
                    | _ -> false
                let mutable moreFactories = true
                while moreFactories do
                    moreFactories <- false
                    for node in root.DescendantNodes() do
                        match node with
                        | :? MethodDeclarationSyntax as method when method.Identifier.ValueText.StartsWith("Native",System.StringComparison.Ordinal) && not (isNull method.Body) ->
                            let symbol = model.GetDeclaredSymbol(method) |> nonNull
                            if symbol.ContainingType.Name=className && symbol.DeclaredAccessibility=Accessibility.Private && inertStatement (nonNull method.Body) then
                                if factories.Add symbol then moreFactories <- true
                        | _ -> ()
                let identifiers = root.DescendantNodes() |> Seq.choose (function :? IdentifierNameSyntax as name -> Some name | _ -> None) |> Seq.toArray
                let referencesBySymbol = Dictionary<ISymbol,ResizeArray<IdentifierNameSyntax>>(SymbolEqualityComparer.Default)
                for name in identifiers do
                    let symbol = model.GetSymbolInfo(name).Symbol
                    if not (isNull symbol) then
                        match referencesBySymbol.TryGetValue(nonNull symbol) with
                        | true, names -> names.Add name
                        | _ -> referencesBySymbol.Add(nonNull symbol,ResizeArray([name]))
                let referencesTo symbol =
                    match referencesBySymbol.TryGetValue symbol with true,names -> names.ToArray() | _ -> [||]
                for node in root.DescendantNodes() do
                    match node with
                    | :? MethodDeclarationSyntax as method when method.Identifier.ValueText.StartsWith("Native",System.StringComparison.Ordinal) ->
                        let symbol = model.GetDeclaredSymbol(method) |> nonNull
                        if symbol.ContainingType.Name=className && symbol.DeclaredAccessibility=Accessibility.Private then
                            let references = referencesTo symbol
                            // Method-group conversions require the original delegate signature.
                            if references |> Array.forall (fun name -> match name.Parent with :? InvocationExpressionSyntax as call -> obj.ReferenceEquals(call.Expression,name) | _ -> false) then
                                let removed = symbol.Parameters |> Seq.mapi (fun index parameter -> index,parameter) |> Seq.choose (fun (index,parameter) ->
                                    let used = referencesTo parameter |> Array.isEmpty |> not
                                    let safe = references |> Array.forall (fun name -> inert (((nonNull name.Parent) :?> InvocationExpressionSyntax).ArgumentList.Arguments.[index].Expression))
                                    if not used && safe then Some index else None) |> Set.ofSeq
                                if not removed.IsEmpty then parameters.Add(symbol,removed)
                    | _ -> ()
                // Local closure cycles are not roots. Follow their capture dependencies
                // from actual uses rather than counting self references as liveness.
                let candidates = Dictionary<ISymbol,ResizeArray<ExpressionSyntax>>(SymbolEqualityComparer.Default)
                for node in root.DescendantNodes() do
                    match node with
                    | :? LocalDeclarationStatementSyntax as statement when statement.Declaration.Variables.Count=1 ->
                        let variable = statement.Declaration.Variables.[0]
                        if not (isNull variable.Initializer) && inert (nonNull variable.Initializer).Value then
                            let symbol = model.GetDeclaredSymbol(variable) |> nonNull
                            if symbol.ContainingType.Name=className then candidates.Add(symbol,ResizeArray([ (nonNull variable.Initializer).Value ]))
                    | _ -> ()
                for name in identifiers do
                    let symbol = model.GetSymbolInfo(name).Symbol
                    if not (isNull symbol) && candidates.ContainsKey(nonNull symbol) then
                        match name.Parent with
                        | :? AssignmentExpressionSyntax as assignment when obj.ReferenceEquals(assignment.Left,name) ->
                            if assignment.IsKind SyntaxKind.SimpleAssignmentExpression && inert assignment.Right then candidates.[nonNull symbol].Add assignment.Right
                            else candidates.Remove(nonNull symbol) |> ignore
                        | :? ArgumentSyntax as argument when not (argument.RefKindKeyword.IsKind SyntaxKind.None) -> candidates.Remove(nonNull symbol) |> ignore
                        | _ -> ()
                let live = HashSet<ISymbol>(SymbolEqualityComparer.Default)
                let dependencies = Dictionary<ISymbol,HashSet<ISymbol>>(SymbolEqualityComparer.Default)
                for pair in candidates do
                    dependencies.Add(pair.Key,HashSet<ISymbol>(SymbolEqualityComparer.Default))
                for name in identifiers do
                    let symbol = model.GetSymbolInfo(name).Symbol
                    if not (isNull symbol) && candidates.ContainsKey(nonNull symbol) then
                        let write = match name.Parent with :? AssignmentExpressionSyntax as assignment -> obj.ReferenceEquals(assignment.Left,name) | _ -> false
                        if not write then
                            let owner = candidates |> Seq.tryFind (fun pair -> pair.Value |> Seq.exists (fun expression -> expression.Span.Contains name.Span))
                            match owner with
                            | Some pair -> dependencies.[pair.Key].Add(nonNull symbol) |> ignore
                            | None -> live.Add(nonNull symbol) |> ignore
                let mutable changed = true
                while changed do
                    changed <- false
                    for symbol in Seq.toArray live do
                        for dependency in dependencies.[symbol] do
                            if live.Add dependency then changed <- true
                for symbol in candidates.Keys do
                    if not (live.Contains symbol) then dead.Add symbol |> ignore
            if parameters.Count=0 && dead.Count=0 then compilation else
                let trees = compilation.SyntaxTrees |> Seq.map (fun tree ->
                    let root = Rewrite(compilation.GetSemanticModel(tree,false),parameters,dead).Visit(tree.GetRoot()) |> nonNull
                    CSharpSyntaxTree.Create(root :?> CSharpSyntaxNode,tree.Options :?> CSharpParseOptions,tree.FilePath,tree.Encoding)) |> Seq.toArray
                iterate (compilation.RemoveAllSyntaxTrees().AddSyntaxTrees(trees))
        iterate input

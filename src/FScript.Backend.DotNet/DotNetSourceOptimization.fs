namespace FScript.Compiler

open System
open System.Collections.Generic
open System.Threading
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax

/// Assembly-only lowering. The source target never runs these rewrites.
module internal DotNetSourceOptimization =
    let rec private unwrap (expression: ExpressionSyntax) =
        match expression with
        | :? ParenthesizedExpressionSyntax as value -> unwrap value.Expression
        | :? CastExpressionSyntax as value -> unwrap value.Expression
        | _ -> expression

    let private boolean expression =
        match unwrap expression with
        | :? LiteralExpressionSyntax as value when value.IsKind SyntaxKind.TrueLiteralExpression -> Some true
        | :? LiteralExpressionSyntax as value when value.IsKind SyntaxKind.FalseLiteralExpression -> Some false
        | _ -> None

    type private Lowering(model: SemanticModel, className: string) =
        inherit CSharpSyntaxRewriter()
        let lifted=ResizeArray<MemberDeclarationSyntax>()
        let mutable next=0
        let mutable insideProgram=false

        override this.VisitClassDeclaration(node) =
            let previous=insideProgram
            insideProgram <- node.Identifier.ValueText=className
            let rewritten=(base.VisitClassDeclaration(node) |> nonNull) :?> ClassDeclarationSyntax
            let result=if insideProgram then rewritten.AddMembers(lifted.ToArray()) else rewritten
            insideProgram <- previous
            result

        override this.VisitRecordDeclaration(node) =
            let rewritten=(base.VisitRecordDeclaration(node) |> nonNull) :?> RecordDeclarationSyntax
            let kind=if node.ClassOrStructKeyword.IsKind SyntaxKind.StructKeyword then "struct" else "class"
            let parameters=if isNull rewritten.ParameterList then [] else (nonNull rewritten.ParameterList).Parameters |> Seq.toList
            let properties=parameters |> List.map (fun parameter -> $"public {parameter.Type} {parameter.Identifier} {{ get; }}") |> String.concat "\n"
            let constructor=
                if parameters.IsEmpty then "" else
                    let signature=parameters |> List.map _.ToFullString() |> String.concat ", "
                    let assignments=parameters |> List.map (fun parameter -> $"this.{parameter.Identifier}={parameter.Identifier};") |> String.concat " "
                    $"public {node.Identifier}({signature}) {{ {assignments} }}"
            let members=rewritten.Members |> Seq.map _.ToFullString() |> String.concat "\n"
            // FScript equality and formatting have dedicated helpers; synthesized CLR record
            // equality, cloning, printing, and deconstruction would all be unused baggage.
            SyntaxFactory.ParseMemberDeclaration($"{node.Modifiers} {kind} {node.Identifier}{node.TypeParameterList} {node.BaseList} {node.ConstraintClauses} {{ {properties} {constructor} {members} }}")

        override this.VisitBinaryExpression(node) =
            let rewritten=(base.VisitBinaryExpression(node) |> nonNull) :?> BinaryExpressionSyntax
            let left,right=boolean rewritten.Left,boolean rewritten.Right
            match rewritten.Kind(),left,right with
            | SyntaxKind.LogicalAndExpression,Some true,_
            | SyntaxKind.LogicalOrExpression,Some false,_ -> rewritten.Right :> SyntaxNode
            | SyntaxKind.LogicalAndExpression,_,Some true
            | SyntaxKind.LogicalOrExpression,_,Some false -> rewritten.Left :> SyntaxNode
            | SyntaxKind.LogicalAndExpression,Some false,_
            | SyntaxKind.LogicalOrExpression,Some true,_ -> rewritten.Left :> SyntaxNode
            | _ -> rewritten

        override this.VisitIfStatement(node) =
            let rewritten=(base.VisitIfStatement(node) |> nonNull) :?> IfStatementSyntax
            match boolean rewritten.Condition with
            | Some true -> rewritten.Statement :> SyntaxNode
            | Some false -> if isNull rewritten.Else then SyntaxFactory.EmptyStatement() :> SyntaxNode else (nonNull rewritten.Else).Statement
            | None -> rewritten

        override this.VisitConditionalExpression(node) =
            let rewritten=(base.VisitConditionalExpression(node) |> nonNull) :?> ConditionalExpressionSyntax
            match boolean rewritten.Condition,boolean rewritten.WhenTrue,boolean rewritten.WhenFalse with
            | Some true,_,_ -> rewritten.WhenTrue :> SyntaxNode
            | Some false,_,_ -> rewritten.WhenFalse :> SyntaxNode
            | _,Some true,Some false -> rewritten.Condition :> SyntaxNode
            | _,Some false,Some true -> SyntaxFactory.PrefixUnaryExpression(SyntaxKind.LogicalNotExpression,SyntaxFactory.ParenthesizedExpression(rewritten.Condition))
            | _ -> rewritten

        override this.VisitInvocationExpression(node) =
            let rewritten=(base.VisitInvocationExpression(node) |> nonNull) :?> InvocationExpressionSyntax
            let target=
                match unwrap node.Expression with
                | :? MemberAccessExpressionSyntax as memberAccess when memberAccess.Name.Identifier.ValueText="Invoke" -> unwrap memberAccess.Expression
                | value -> value
            match target with
            | :? ParenthesizedLambdaExpressionSyntax as lambda when insideProgram && node.ArgumentList.Arguments.Count=0 && lambda.ParameterList.Parameters.Count=0 ->
                let flow=
                    match lambda.Body with
                    | :? BlockSyntax as block -> model.AnalyzeDataFlow(block :> StatementSyntax)
                    | :? ExpressionSyntax as expression -> model.AnalyzeDataFlow(expression)
                    | _ -> null
                if isNull flow || not (nonNull flow).Succeeded then rewritten else
                    let flow=nonNull flow
                    let captures=flow.DataFlowsIn |> Seq.filter (fun symbol -> symbol.Kind=SymbolKind.Local || symbol.Kind=SymbolKind.Parameter) |> Seq.sortBy _.Name |> Seq.toList
                    let writesCapture=captures |> List.exists (fun symbol -> flow.WrittenInside |> Seq.exists (fun written -> SymbolEqualityComparer.Default.Equals(written,symbol)))
                    let delegateType=model.GetTypeInfo(lambda).ConvertedType
                    match delegateType with
                    | :? INamedTypeSymbol as delegateType when not writesCapture ->
                        let invoke=delegateType.DelegateInvokeMethod |> nonNull
                        let returnType=invoke.ReturnType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)
                        let parameters=captures |> List.map (fun symbol ->
                            let t=match symbol with :? ILocalSymbol as local -> local.Type | :? IParameterSymbol as parameter -> parameter.Type | _ -> invalidOp "Invalid capture."
                            t.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)+" @"+symbol.Name) |> String.concat ", "
                        let arguments=captures |> List.map (fun symbol -> "@"+symbol.Name) |> String.concat ", "
                        let visitedTarget=
                            match unwrap rewritten.Expression with
                            | :? MemberAccessExpressionSyntax as memberAccess -> unwrap memberAccess.Expression
                            | value -> value
                        let visitedLambda=visitedTarget :?> ParenthesizedLambdaExpressionSyntax
                        let body=
                            match visitedLambda.Body with
                            | :? BlockSyntax as block -> block.ToFullString()
                            | value -> "{ return "+value.ToFullString()+"; }"
                        let name="NativeBlock"+string next
                        next <- next+1
                        lifted.Add(SyntaxFactory.ParseMemberDeclaration($"private static {returnType} {name}({parameters}) {body}") |> nonNull)
                        SyntaxFactory.ParseExpression($"{name}({arguments})")
                    | _ -> rewritten
            | _ -> rewritten

    type private Pruning(retained: HashSet<SyntaxNode>) =
        inherit CSharpSyntaxRewriter()
        override this.VisitClassDeclaration(node) =
            if retained.Contains node then base.VisitClassDeclaration(node) else null
        override this.VisitStructDeclaration(node) =
            if retained.Contains node then base.VisitStructDeclaration(node) else null
        override this.VisitRecordDeclaration(node) =
            if retained.Contains node then base.VisitRecordDeclaration(node) else null
        override this.VisitEnumDeclaration(node) =
            if retained.Contains node then base.VisitEnumDeclaration(node) else null
        override this.VisitMethodDeclaration(node) =
            if retained.Contains node then base.VisitMethodDeclaration(node) else null
        override this.VisitPropertyDeclaration(node) =
            if retained.Contains node then base.VisitPropertyDeclaration(node) else null
        override this.VisitFieldDeclaration(node) =
            if retained.Contains node then base.VisitFieldDeclaration(node) else null
        override this.VisitConstructorDeclaration(node) =
            if retained.Contains node then base.VisitConstructorDeclaration(node) else null

    type private UnreadFields(model: SemanticModel, unread: HashSet<ISymbol>) =
        inherit CSharpSyntaxRewriter()
        let mutable emptyInitializer=false
        override this.VisitMethodDeclaration(node) =
            let result=(base.VisitMethodDeclaration(node) |> nonNull) :?> MethodDeclarationSyntax
            if node.Identifier.ValueText="Initialize" && not (isNull result.Body) then
                emptyInitializer <- (nonNull result.Body).Statements |> Seq.forall (fun statement -> statement :? EmptyStatementSyntax || statement :? ReturnStatementSyntax)
            result
        override this.VisitExpressionStatement(node) =
            match node.Expression with
            | :? AssignmentExpressionSyntax as assignment when assignment.IsKind SyntaxKind.SimpleAssignmentExpression ->
                let target=model.GetSymbolInfo(assignment.Left).Symbol
                if not (isNull target) && unread.Contains(nonNull target) then
                    match unwrap assignment.Right with
                    | :? LambdaExpressionSyntax -> SyntaxFactory.EmptyStatement() :> SyntaxNode
                    | _ -> SyntaxFactory.ParseStatement($"_ = {assignment.Right.ToFullString()};")
                elif emptyInitializer && assignment.Left.ToString()="_" && assignment.Right.ToString()="Initialization.Value" then
                    SyntaxFactory.EmptyStatement() :> SyntaxNode
                else base.VisitExpressionStatement(node) |> nonNull
            | _ -> base.VisitExpressionStatement(node) |> nonNull

    let optimize className isLibrary (compilation: CSharpCompilation) =
        let lowered=
            compilation.SyntaxTrees |> Seq.map (fun tree ->
                let root=Lowering(compilation.GetSemanticModel(tree, false),className).Visit(tree.GetRoot()) |> nonNull
                CSharpSyntaxTree.Create(root :?> CSharpSyntaxNode, tree.Options :?> CSharpParseOptions,tree.FilePath,tree.Encoding) ) |> Seq.toList
        let compilation=compilation.RemoveAllSyntaxTrees().AddSyntaxTrees(lowered)
        let candidates=HashSet<ISymbol>(SymbolEqualityComparer.Default)
        let reads=HashSet<ISymbol>(SymbolEqualityComparer.Default)
        for tree in compilation.SyntaxTrees do
            let model=compilation.GetSemanticModel(tree,false)
            for node in tree.GetRoot().DescendantNodes() do
                match node with
                | :? VariableDeclaratorSyntax as variable ->
                    match model.GetDeclaredSymbol(variable) with
                    | :? IFieldSymbol as field when field.IsStatic && field.DeclaredAccessibility=Accessibility.Private && field.ContainingType.Name=className -> candidates.Add(field) |> ignore
                    | _ -> ()
                | :? IdentifierNameSyntax as name ->
                    let value=model.GetSymbolInfo(name).Symbol
                    if not (isNull value) then
                        let onlyWrite=match name.Parent with :? AssignmentExpressionSyntax as assignment when assignment.IsKind SyntaxKind.SimpleAssignmentExpression -> obj.ReferenceEquals(assignment.Left,name) | _ -> false
                        if not onlyWrite then reads.Add(nonNull value) |> ignore
                | _ -> ()
        candidates.ExceptWith(reads)
        let stripped=compilation.SyntaxTrees |> Seq.map (fun tree ->
            let root=UnreadFields(compilation.GetSemanticModel(tree,false),candidates).Visit(tree.GetRoot()) |> nonNull
            CSharpSyntaxTree.Create(root :?> CSharpSyntaxNode,tree.Options :?> CSharpParseOptions,tree.FilePath,tree.Encoding)) |> Seq.toList
        let compilation=compilation.RemoveAllSyntaxTrees().AddSyntaxTrees(stripped)
        let retained=HashSet<SyntaxNode>()
        let visited=HashSet<ISymbol>(SymbolEqualityComparer.Default)
        let queue=Queue<SyntaxNode>()
        let models=compilation.SyntaxTrees |> Seq.map (fun tree -> tree,compilation.GetSemanticModel(tree,false)) |> dict
        let rec retain (node: SyntaxNode) =
            if retained.Add node then
                queue.Enqueue node
                node.Ancestors() |> Seq.tryPick (function :? BaseTypeDeclarationSyntax as parent -> Some(parent :> SyntaxNode) | _ -> None) |> Option.iter retain
        let rec symbol (value: ISymbol) =
            if visited.Add value then
                match value with
                | :? IArrayTypeSymbol as array -> symbol array.ElementType
                | :? INamedTypeSymbol as named ->
                    named.TypeArguments |> Seq.iter symbol
                    // Instance constructors and interface implementations participate in runtime dispatch.
                    if SymbolEqualityComparer.Default.Equals(named.ContainingAssembly,compilation.Assembly) then
                        for memberSymbol in named.GetMembers() do
                            match memberSymbol with
                            | :? IMethodSymbol as method when method.MethodKind=MethodKind.Constructor || method.MethodKind=MethodKind.StaticConstructor || method.IsOverride || method.IsVirtual || (not method.IsStatic && named.AllInterfaces.Length<>0) -> symbol method
                            | :? IPropertySymbol as property when property.IsOverride || property.IsVirtual || (not property.IsStatic && named.AllInterfaces.Length<>0) -> symbol property
                            | _ -> ()
                | :? IMethodSymbol as method ->
                    symbol method.ReturnType
                    method.Parameters |> Seq.iter (fun parameter -> symbol parameter.Type)
                    method.TypeArguments |> Seq.iter symbol
                | :? IFieldSymbol as field -> symbol field.Type
                | :? IPropertySymbol as property -> symbol property.Type
                | _ -> ()
                if not (isNull value.ContainingType) then symbol value.ContainingType
                for reference in value.OriginalDefinition.DeclaringSyntaxReferences do
                    let declaration=reference.GetSyntax(CancellationToken.None)
                    declaration.AncestorsAndSelf() |> Seq.tryPick (function :? MemberDeclarationSyntax as memberNode -> Some(memberNode :> SyntaxNode) | _ -> None) |> Option.iter retain
        for tree in compilation.SyntaxTrees do
            let model=models.[tree]
            for node in tree.GetRoot().DescendantNodes() do
                match node with
                | :? ClassDeclarationSyntax as typ when typ.Identifier.ValueText=className ->
                    for memberNode in typ.Members do
                        match memberNode with
                        | :? MethodDeclarationSyntax as method when method.Identifier.ValueText="Run" || method.Identifier.ValueText="FormatResult" || (not isLibrary && method.Identifier.ValueText="Main") -> retain method
                        | :? PropertyDeclarationSyntax as property when property.Modifiers |> Seq.exists (fun token -> token.IsKind SyntaxKind.PublicKeyword) -> retain property
                        | _ -> ()
                | _ -> ()
        while queue.Count<>0 do
            let node=queue.Dequeue()
            let model=models.[node.SyntaxTree]
            let nodes=
                match node with
                | :? BaseTypeDeclarationSyntax ->
                    node.DescendantNodesAndSelf(fun child -> obj.ReferenceEquals(child,node) || not (child :? MemberDeclarationSyntax))
                | _ -> node.DescendantNodesAndSelf()
            for child in nodes do
                match child with
                | :? ExpressionSyntax | :? TypeSyntax | :? ConstructorInitializerSyntax ->
                    let info=model.GetSymbolInfo child
                    if not (isNull info.Symbol) then symbol (nonNull info.Symbol)
                    info.CandidateSymbols |> Seq.iter symbol
                    let t=model.GetTypeInfo child
                    if not (isNull t.Type) then symbol (nonNull t.Type)
                | _ -> ()
        let trees=compilation.SyntaxTrees |> Seq.map (fun tree ->
            let root=Pruning(retained).Visit(tree.GetRoot()) |> nonNull
            CSharpSyntaxTree.Create(root :?> CSharpSyntaxNode,tree.Options :?> CSharpParseOptions,tree.FilePath,tree.Encoding)) |> Seq.toList
        compilation.RemoveAllSyntaxTrees().AddSyntaxTrees(trees)

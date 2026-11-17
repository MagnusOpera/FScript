namespace FScript.Runtime

open FScript.Language

module Registry =
    let internal mailbox =
        let registration = MailboxProvider.register () |> ExtensionBridge.toLanguageRegistration
        let functions = registration.Functions |> List.map (fun fn -> { fn with RequiredPermissions = HostPermissions.Threading })
        { registration with Functions = functions }

    /// Default providers that also supply typed compiled linkage.
    let compiledProviders () =
        [ mailbox.Functions |> List.map _.Name, typeof<CompiledMailbox>.GetMethod("Start") |> nonNull
          ["Process.defaults"; "Process.exec"], typeof<FScript.Runtime.CompiledProcess>.GetMethod("Exec") |> nonNull ]

    let all (ctx: HostContext) : ExternalFunction list =
        [ ProcessExterns.defaults
          ProcessExterns.exec ctx
          FsExterns.read_text ctx
          FsExterns.exists ctx
          FsExterns.entry_kind ctx
          FsExterns.create_directory ctx
          FsExterns.write_text ctx
          FsExterns.combine_path
          FsExterns.parent_directory
          FsExterns.extension
          FsExterns.file_name_without_extension
          FsExterns.glob ctx
          FsExterns.enumerate_files ctx
          RegexExterns.match_groups
          HashExterns.md5
          GuidExterns.new_guid
          ConsoleExterns.write_line
          ConsoleExterns.read_line
          JsonExterns.deserialize
          JsonExterns.serialize
          XmlExterns.query_values ] @ PureExterns.functions @ TaskExterns.all @ mailbox.Functions
        |> List.filter (fun fn -> HostInterop.allows ctx.Permissions fn.RequiredPermissions)

    /// Create an interpreter registration with exactly the host's grants.
    let registration (context: HostContext) =
        let registration = HostInterop.ofExternsWithPermissions context.Permissions (all context)
        { registration with
            Unions = ProcessSignatures.resultDescriptor :: (if HostInterop.allows context.Permissions HostPermissions.IO then [FsExterns.kindType] else [])
            Types = ProcessSignatures.optionsDescriptor :: (if HostInterop.allows context.Permissions HostPermissions.Threading then mailbox.Types else []) }

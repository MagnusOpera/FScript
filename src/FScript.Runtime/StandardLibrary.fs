namespace FScript.Runtime

open FScript.Language

module internal StandardLibrary =
    let internal reference =
        let registration = RefProvider.register () |> ExtensionBridge.toLanguageRegistration
        { registration with Functions = registration.Functions |> List.map (fun fn -> { fn with RequiredPermissions = HostPermissions.None }) }

    let internal mailbox =
        let registration = MailboxProvider.register () |> ExtensionBridge.toLanguageRegistration
        let functions = registration.Functions |> List.map (fun fn -> { fn with RequiredPermissions = HostPermissions.Threading })
        { registration with Functions = functions }

    let all (ctx: HostContext) (policy: FileSystemPolicy) : ExternalFunction list =
        [ ProcessExterns.defaults
          ProcessExterns.exec ctx
          FsExterns.read_textWithPolicy policy
          FsExterns.existsWithPolicy policy
          FsExterns.entry_kindWithPolicy policy
          FsExterns.create_directoryWithPolicy policy
          FsExterns.write_textWithPolicy policy
          FsExterns.combine_path
          FsExterns.parent_directory
          FsExterns.extension
          FsExterns.file_name_without_extension
          FsExterns.globWithPolicy policy
          FsExterns.enumerate_filesWithPolicy policy
          RegexExterns.match_groups
          HashExterns.md5
          GuidExterns.new_guid
          ConsoleExterns.write_line
          ConsoleExterns.read_line
          JsonExterns.deserialize
          JsonExterns.serialize
          XmlExterns.query_values ] @ PureExterns.functions @ TaskExterns.all @ mailbox.Functions @ reference.Functions
        |> List.filter (fun fn -> HostInterop.allows ctx.Permissions fn.RequiredPermissions)

    /// Create an interpreter registration with exactly the host's grants.
    let registration (context: HostContext) policy =
        let registration = HostInterop.ofExternsWithPermissions context.Permissions (all context policy)
        { registration with
            Unions = ProcessSignatures.resultDescriptor :: (if HostInterop.allows context.Permissions HostPermissions.IO then [FsExterns.kindType] else [])
            Types = ProcessSignatures.optionsDescriptor :: reference.Types @ (if HostInterop.allows context.Permissions HostPermissions.Threading then mailbox.Types else []) }

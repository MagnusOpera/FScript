namespace FScript.Runtime

/// Interpreter host permissions; the implementation-independent SDK stays unchanged.
type HostPermissions = FScript.Language.HostPermissions

type HostContext =
    { Permissions: FScript.Language.HostPermissions
      RootDirectory: string
      DeniedPathGlobs: string list }

module HostContext =
    /// Default-deny context; the host must explicitly grant effectful capabilities.
    let create rootDirectory : HostContext =
        { Permissions = FScript.Language.HostPermissions.None
          RootDirectory = rootDirectory
          DeniedPathGlobs = [] }

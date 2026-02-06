namespace FScript.Host

open FScript.Core

module Registry =
    let all (ctx: HostContext) : ExternalFunction list =
        [ FsExterns.read_text ctx
          FsExterns.glob ctx
          RegexExterns.match_groups
          HashExterns.md5
          GuidExterns.new_guid
          MapExterns.empty
          MapExterns.add
          MapExterns.tryFind
          MapExterns.containsKey
          MapExterns.remove
          JsonExterns.deserialize
          XmlExterns.values ]

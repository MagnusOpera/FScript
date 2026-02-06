namespace FScript.Host

open FScript.Core

module Registry =
    let all (ctx: HostContext) : ExternalFunction list =
        [ FsExterns.read_text ctx
          FsExterns.glob ctx
          RegexExterns.match_groups
          HashExterns.md5
          GuidExterns.new_guid
          PrintExterns.print
          MapExterns.empty
          MapExterns.add
          MapExterns.tryGet
          MapExterns.tryFind
          MapExterns.containsKey
          MapExterns.remove
          ListExterns.map
          ListExterns.iter
          ListExterns.tryHead
          ListExterns.tail
          ListExterns.append
          JsonExterns.deserialize
          XmlExterns.values ]

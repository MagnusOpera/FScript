namespace FScript.Runtime

open FScript.Language

module Registry =
    let all (ctx: HostContext) : ExternalFunction list =
        [ FsExterns.read_text ctx
          FsExterns.combine_path
          FsExterns.parent_directory
          FsExterns.extension
          FsExterns.file_name_without_extension
          FsExterns.glob ctx
          FsExterns.enumerate_files ctx
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
          ListExterns.choose
          ListExterns.collect
          ListExterns.contains
          ListExterns.distinct
          ListExterns.exists
          ListExterns.fold
          ListExterns.filter
          ListExterns.iter
          ListExterns.rev
          ListExterns.length
          ListExterns.tryFind
          ListExterns.tryFindIndex
          ListExterns.tryHead
          ListExterns.tail
          ListExterns.append
          OptionExterns.get
          OptionExterns.defaultValue
          OptionExterns.defaultWith
          OptionExterns.isNone
          OptionExterns.isSome
          OptionExterns.map
          JsonExterns.deserialize
          XmlExterns.values ]

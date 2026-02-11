namespace FScript.Runtime

open FScript.Language

module Registry =
    let all (ctx: HostContext) : ExternalFunction list =
        [ FsExterns.read_text ctx
          FsExterns.exists ctx
          FsExterns.is_file ctx
          FsExterns.is_directory ctx
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
          PrintExterns.print
          JsonExterns.deserialize
          XmlExterns.values ]

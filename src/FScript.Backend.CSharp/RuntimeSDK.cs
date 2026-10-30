// SDK value transport at compiled FFI boundaries; no interpreter or script host.
using System;
using System.Linq;
using System.Threading.Tasks;
using System.IO;
using System.Text;
using System.Text.Json;
using System.Globalization;
using Microsoft.FSharp.Core;
using Microsoft.FSharp.Collections;
using FScript.Extensibility;
using SV = FScript.Extensibility.ScriptValue;
using ST = FScript.Extensibility.ScriptType;

namespace __NAMESPACE__;

internal static class SdkInterop
{
    private sealed class Callback(Func<SV,SV> callback) : ICallableHandle { public SV Apply(SV argument) => callback(argument); }
    private sealed class Pending(Task<SV> task) : ITaskHandle { public Task<SV> Value => task; }
    public static SV Callable(Func<SV,SV> fn) => SV.NewVCallable(new Callback(fn));
    public static SV TaskValue(Task<SV> task) => SV.NewVTask(new Pending(task));
    public static SV Apply(SV fn, SV argument) => fn is SV.VCallable callable && callable.Item is Callback callback ? callback.Apply(argument) : throw new ExtensionException("Foreign callable does not belong to this compiled program.");
    public static Task<SV> PendingTask(SV value) => value is SV.VTask pending && pending.Item is Pending task ? task.Value : throw new ExtensionException("Foreign task does not belong to this compiled program.");
    public static SV Await(SV value) => FsTasks.Await(PendingTask(value));
    private static readonly ExternalCallContext Context = new(
        FuncConvert.FromFunc<SV,FSharpFunc<SV,SV>>(fn => FuncConvert.FromFunc<SV,SV>(argument => Apply(fn,argument))),
        FuncConvert.FromFunc<SV,SV>(fn => TaskValue(FsTasks.Spawn(_ => Apply(fn,SV.VUnit)))),
        FuncConvert.FromFunc<SV,SV>(value => Await(value)),
        FuncConvert.FromAction(() => { }));
    public static SV Invoke(ExternalFunction fn, SV[] arguments) => fn.Impl.Invoke(Context).Invoke(ListModule.OfArray(arguments));
    public static ExternalFunction Find(ExtensionRegistration registration, string name) => registration.Functions.First(fn => fn.Name == name);
    public static FSharpOption<SV> Some(SV value) => FSharpOption<SV>.Some(value);
    public static FSharpMap<string,SV> Fields(Tuple<string,SV>[] values) => MapModule.OfSeq(values);
    private static SV Optional(Func<SV> action) { try { return SV.NewVOption(Some(action())); } catch { return SV.NewVOption(null); } }
    private static string Text(SV value) => ((SV.VString)value).Item;
    private static SV Strings(string[] values) => SV.NewVList(ListModule.OfArray(values.Select(SV.NewVString).ToArray()));
    private static long[] Bytes(SV value) => ((SV.VList)value).Item.Select(item => ((SV.VInt)item).Item).ToArray();
    private static SV ByteList(long[] bytes) => SV.NewVList(ListModule.OfArray(bytes.Select(SV.NewVInt).ToArray()));
    private static SV TextOption(Option<string> value) => SV.NewVOption(value.HasValue ? Some(SV.NewVString(value.Value)) : null);
    private static SV ByteOption(Option<long[]> value) => SV.NewVOption(value.HasValue ? Some(ByteList(value.Value)) : null);
    public static SV Default(string name, ExternalCallContext context, SV[] args)
    {
        switch(name)
        {
            case "Console.writeLine": Console.WriteLine(Text(args[0])); return SV.VUnit;
            case "Console.readLine": var line=Console.ReadLine(); return SV.NewVOption(line is null ? null : Some(SV.NewVString(line)));
            case "Fs.readText": return Optional(() => SV.NewVString(File.ReadAllText(Text(args[0]))));
            case "Fs.exists": return SV.NewVBool(File.Exists(Text(args[0])) || Directory.Exists(Text(args[0])));
            case "Fs.createDirectory": return SV.NewVBool(FsRuntime.TryEffect(() => Directory.CreateDirectory(Text(args[0]))));
            case "Fs.writeText": return SV.NewVBool(FsRuntime.TryEffect(() => File.WriteAllText(Text(args[0]),Text(args[1]))));
            case "Fs.combinePath": return SV.NewVString(Path.Combine(Text(args[0]),Text(args[1])));
            case "Fs.parentDirectory": var parent=Path.GetDirectoryName(Text(args[0])); return SV.NewVOption(string.IsNullOrEmpty(parent) ? null : Some(SV.NewVString(parent)));
            case "Fs.extension": var extension=Path.GetExtension(Text(args[0])); return SV.NewVOption(string.IsNullOrEmpty(extension) ? null : Some(SV.NewVString(extension)));
            case "Fs.fileNameWithoutExtension": return SV.NewVString(Path.GetFileNameWithoutExtension(Text(args[0])));
            case "Fs.glob": var files=FsRuntime.Files(Directory.GetCurrentDirectory(),Text(args[0]),true); return SV.NewVOption(files.HasValue ? Some(Strings(files.Value)) : null);
            case "Fs.enumerateFiles": var entries=FsRuntime.Files(Text(args[0]),Text(args[1]),false); return SV.NewVOption(entries.HasValue ? Some(Strings(entries.Value)) : null);
            case "Fs.kind":
                var path=Text(args[0]); var kind=File.Exists(path) ? "File" : Directory.Exists(path) ? "Directory" : "Missing";
                return SV.NewVUnionCase("FsKind",kind,kind == "Missing" ? null : Some(SV.NewVString(Path.GetRelativePath(Directory.GetCurrentDirectory(),Path.GetFullPath(path)).Replace('\\','/'))));
            case "Regex.matchGroups": var groups=FsRuntime.MatchGroups(Text(args[0]),Text(args[1])); return SV.NewVOption(groups.HasValue ? Some(Strings(groups.Value)) : null);
            case "Guid.new": return SV.NewVOption(Some(SV.NewVString(Guid.NewGuid().ToString())));
            case "Encoding.utf8Encode": return ByteOption(FsPure.Utf8Encode(Text(args[0])));
            case "Encoding.utf8Decode": return TextOption(FsPure.Utf8Decode(Bytes(args[0])));
            case "Encoding.base64Encode": return TextOption(FsPure.Base64Encode(Bytes(args[0])));
            case "Encoding.base64Decode": return ByteOption(FsPure.Base64Decode(Text(args[0])));
            case "Encoding.hexEncode": return TextOption(FsPure.HexEncode(Bytes(args[0])));
            case "Encoding.hexDecode": return ByteOption(FsPure.HexDecode(Text(args[0])));
            case "Encoding.uriEncode": return TextOption(FsPure.UriEncode(Text(args[0])));
            case "Encoding.uriDecode": return TextOption(FsPure.UriDecode(Text(args[0])));
            case "Hash.sha256": return TextOption(FsPure.Sha256(Text(args[0])));
            case "Hash.sha256Bytes": return TextOption(FsPure.Sha256Bytes(Bytes(args[0])));
            case "Hash.md5": return Optional(() => SV.NewVString(Convert.ToHexString(System.Security.Cryptography.MD5.HashData(Encoding.UTF8.GetBytes(Text(args[0])))).ToLowerInvariant()));
            case "Json.serialize": return Optional(() => SV.NewVString(EncodeJson(args[0])));
            case "Json.deserialize": return Optional(() => { using var document=JsonDocument.Parse(Text(args[1])); return DecodeJson(((SV.VTypeToken)args[0]).Item,document.RootElement); });
            case "Xml.queryValues": var values=FsRuntime.QueryXml(Text(args[0]),Text(args[1])); return SV.NewVOption(values.HasValue ? Some(Strings(values.Value)) : null);
            case "Task.spawn": return context.SpawnTask.Invoke(args[0]);
            case "Task.await": return context.AwaitTask.Invoke(args[0]);
            case "Task.delay": return TaskValue(FsTasks.Convert(FsTasks.Delay(((SV.VInt)args[0]).Item), _ => SV.VUnit));
            case "Task.fromResult": return TaskValue(FsTasks.FromResult(args[0]));
            case "Task.continueWith": return TaskValue(FsTasks.Continue(PendingTask(args[1]), value => Apply(args[0],value)));
            case "Task.whenAll": return TaskValue(FsTasks.WhenAll(((SV.VList)args[1]).Item.Select(PendingTask), values => Apply(args[0],SV.NewVList(ListModule.OfArray(values)))));
            case "Task.whenAny": return TaskValue(FsTasks.WhenAny(((SV.VList)args[1]).Item.Select(PendingTask), value => Apply(args[0],value)));
            case "Task.awaitAll": return SV.NewVList(ListModule.OfArray(FsTasks.AwaitAll(((SV.VList)args[0]).Item.Select(PendingTask))));
            default: throw new ExtensionException("Unknown compiled default function: " + name);
        }
    }
    private static string EncodeJson(SV value) => value switch
    {
        SV.VInt item => item.Item.ToString(CultureInfo.InvariantCulture),
        SV.VFloat item => item.Item.ToString(CultureInfo.InvariantCulture),
        SV.VBool item => item.Item ? "true" : "false",
        SV.VString item => JsonSerializer.Serialize(item.Item),
        SV.VList item => "["+string.Join(",",item.Item.Select(EncodeJson))+"]",
        SV.VTuple item => "["+string.Join(",",item.Item.Select(EncodeJson))+"]",
        SV.VRecord item => "{"+string.Join(",",item.Item.Select(field=>JsonSerializer.Serialize(field.Key)+":"+EncodeJson(field.Value)))+"}",
        SV.VHostManaged item => "{"+string.Join(",",item.Item.Fields.Select(field=>JsonSerializer.Serialize(field.Key)+":"+EncodeJson(field.Value)))+"}",
        SV.VMap item => "{"+string.Join(",",item.Item.Select(field=>JsonSerializer.Serialize(JsonKey(field.Key))+":"+EncodeJson(field.Value)))+"}",
        SV.VOption item => item.Item is { } some ? EncodeJson(some.Value) : "null",
        _ when value.IsVUnit => "null",
        _ => throw new ExtensionException("Value cannot be serialized as JSON.")
    };
    private static string JsonKey(MapKey value) => value switch { MapKey.MKString s=>s.Item, MapKey.MKInt n=>n.Item.ToString(CultureInfo.InvariantCulture), MapKey.MKBool b=>b.Item ? "true" : "false", _=>throw new ExtensionException("Union keys cannot be JSON properties.") };
    private static SV DecodeJson(ST type, JsonElement value)
    {
        if(type.IsTUnit) return SV.VUnit;
        if(type.IsTString) return SV.NewVString(value.GetString() ?? throw new ExtensionException("Expected string."));
        if(type.IsTInt) return SV.NewVInt(value.GetInt64());
        if(type.IsTFloat) return SV.NewVFloat(value.GetDouble());
        if(type.IsTBool) return SV.NewVBool(value.GetBoolean());
        if(type is ST.TList list) return SV.NewVList(ListModule.OfArray(value.EnumerateArray().Select(item=>DecodeJson(list.Item,item)).ToArray()));
        if(type is ST.TOption option) return SV.NewVOption(value.ValueKind==JsonValueKind.Null ? null : Some(DecodeJson(option.Item,value)));
        if(type is ST.TTuple tuple)
        {
            var items=value.EnumerateArray().ToArray();
            if(items.Length != tuple.Item.Length) throw new ExtensionException("Tuple arity mismatch.");
            return SV.NewVTuple(ListModule.OfArray(tuple.Item.Select((t,index)=>DecodeJson(t,items[index])).ToArray()));
        }
        if(type is ST.TRecord record) return SV.NewVRecord(Fields(record.Item.Select(field=>Tuple.Create(field.Key,value.TryGetProperty(field.Key,out var property) ? DecodeJson(field.Value,property) : field.Value is ST.TOption ? SV.NewVOption(null) : throw new ExtensionException("Missing JSON field."))).ToArray()));
        if(type is ST.TMap map)
        {
            MapKey Key(string text) => map.Item1.IsTString ? MapKey.NewMKString(text) : map.Item1.IsTInt ? MapKey.NewMKInt(long.Parse(text,CultureInfo.InvariantCulture)) : map.Item1.IsTBool ? MapKey.NewMKBool(bool.Parse(text)) : throw new ExtensionException("Unsupported JSON map key.");
            return SV.NewVMap(MapModule.OfSeq(value.EnumerateObject().Select(property=>Tuple.Create(Key(property.Name),DecodeJson(map.Item2,property.Value)))));
        }
        throw new ExtensionException("Unsupported JSON type.");
    }
}

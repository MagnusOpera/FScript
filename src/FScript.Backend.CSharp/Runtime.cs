// Generated FScript support code. Uses only the .NET base class libraries.
using System;
using System.IO;
using System.Linq;
using System.Globalization;
using System.Collections.Generic;
using System.Text.Json;
using System.Threading.Tasks;

namespace __NAMESPACE__;

public readonly record struct Unit;
public readonly record struct TypeToken(string Name);
public readonly record struct Option<T>(bool HasValue, T Value)
{
    public static Option<T> None => default;
    public static Option<T> Some(T value) => new(true, value);
}
public sealed class OrderedMap<K, V>
{
    public readonly record struct Entry(K Key, V Value);
    public Entry[] Entries { get; }
    public OrderedMap() : this(Array.Empty<Entry>()) { }
    public OrderedMap(Entry[] entries) { Entries = entries; }
    public Option<V> Find(K key)
    {
        foreach (var entry in Entries)
            if (EqualityComparer<K>.Default.Equals(entry.Key, key)) return Option<V>.Some(entry.Value);
        return default;
    }
    public OrderedMap<K,V> Add(K key, V value)
    {
        var entries = Entries.Where(e => !EqualityComparer<K>.Default.Equals(e.Key, key))
            .Append(new Entry(key,value)).OrderBy(e => e.Key, Comparer<K>.Default).ToArray();
        return new(entries);
    }
    public OrderedMap<K,V> Remove(K key) => new(Entries.Where(e => !EqualityComparer<K>.Default.Equals(e.Key,key)).ToArray());
}
internal static class FsRuntime
{
    internal static T[] ListBuffer<T>(T[] values) => values;
    internal static int ListOffset<T>(T[] values) => 0;
    // Compact selection masks preserve predicate order and avoid growing element buffers.
    public static T[] Filter<T>(Func<T,bool> predicate,T[] source)
    {
        ReadOnlySpan<T> items=source.AsSpan();
        int length=items.Length;
        int words=length==0 ? 0 : ((length-1)>>5)+1;
        uint[]? masks=words>1 ? new uint[words] : null;
        uint smallMask=0;
        int count=0;
        for(int index=0;index<length;)
        {
            int start=index;
            int size=Math.Min(32,length-index);
            uint mask=0;
            for(int bit=0;bit<size;bit++,index++)
                if(predicate(items[index])) { mask |= 1u << bit; count++; }
            if(masks is null) smallMask=mask;
            else masks[start>>5]=mask;
        }
        if(count==0) return Array.Empty<T>();
        var result=new T[count];
        int destination=0;
        for(int word=0;word<words;word++)
        {
            uint mask=masks is null ? smallMask : masks[word];
            int start=word<<5;
            while(mask!=0)
            {
                int bit=System.Numerics.BitOperations.TrailingZeroCount(mask);
                result[destination++]=items[start+bit];
                mask &= mask-1;
            }
        }
        return result;
    }

    public static string JsonString(string value) => "\"" + JsonEncodedText.Encode(value).ToString() + "\"";
    public static string Quote(string value) => "\"" + value.Replace("\\","\\\\").Replace("\"","\\\"").Replace("\r","\\r").Replace("\n","\\n").Replace("\t","\\t") + "\"";
    public static T Fail<T>(string message) => throw new InvalidOperationException(message);
    public static Unit WriteLine(string value) { Console.WriteLine(value); return default; }
    public static Option<string> ReadLine(Unit _) { var text = Console.ReadLine(); return text is null ? default : Option<string>.Some(text); }
    public static Option<T> Try<T>(Func<T> action) { try { return Option<T>.Some(action()); } catch { return default; } }
    public static bool TryEffect(Action action) { try { action(); return true; } catch { return false; } }
    public static long[] Range(long first, long last)
    {
        var result = new List<long>();
        if (first <= last) { for (long value = first;; value++) { result.Add(value); if (value == last) break; } }
        else { for (long value = first;; value--) { result.Add(value); if (value == last) break; } }
        return result.ToArray();
    }
    public static T[] Tail<T>(T[] values) => values.Length==0 ? throw new InvalidOperationException("List.tail expects a non-empty list") : values.Skip(1).ToArray();
    public static Option<T> At<T>(long index, T[] values) => index >= 0 && index < values.LongLength ? Option<T>.Some(values[index]) : default;
    public static Unit Iter<T>(Func<T,Unit> fn, T[] values) { foreach(var value in values) fn(value); return default; }
    public static S Fold<T,S>(Func<S,Func<T,S>> fn, S state, T[] values) { foreach(var value in values) state = fn(state)(value); return state; }
    public static T[] Distinct<T>(T[] values, Func<T,T,bool> equal)
    {
        var result = new List<T>();
        foreach(var value in values) if (!result.Any(item => equal(item,value))) result.Add(value);
        return result.ToArray();
    }
    public static Option<T> Find<T>(Func<T,bool> fn, T[] values)
    {
        foreach(var value in values) if(fn(value)) return Option<T>.Some(value);
        return default;
    }
    public static Option<long> FindIndex<T>(Func<T,bool> fn, T[] values)
    {
        for(int i=0;i<values.Length;i++) if(fn(values[i])) return Option<long>.Some(i);
        return default;
    }
    public static Option<long> ParseInt(string value) => long.TryParse(value, NumberStyles.Integer, CultureInfo.InvariantCulture, out var result) ? Option<long>.Some(result) : default;
    public static Option<double> ParseFloat(string value) => double.TryParse(value, NumberStyles.Float, CultureInfo.InvariantCulture, out var result) ? Option<double>.Some(result) : default;
    public static Option<bool> ParseBool(string value) => bool.TryParse(value, out var result) ? Option<bool>.Some(result) : default;
    public static string Replace(string oldValue, string newValue, string source) => oldValue.Length == 0 ? source : source.Replace(oldValue, newValue, StringComparison.Ordinal);
    public static Option<long> IndexOf(string needle, string haystack) { long index = haystack.IndexOf(needle, StringComparison.Ordinal); return index < 0 ? default : Option<long>.Some(index); }
    public static Option<string> Substring(long start, long length, string value) => start < 0 || length < 0 || start > value.Length || length > value.Length-start ? default : Option<string>.Some(value.Substring((int)start,(int)length));
    public static S MapFold<K,V,S>(Func<S,Func<K,Func<V,S>>> fn, S state, OrderedMap<K,V> values) { foreach(var e in values.Entries) state=fn(state)(e.Key)(e.Value); return state; }
    public static Unit MapIter<K,V>(Func<K,Func<V,Unit>> fn, OrderedMap<K,V> values) { foreach(var e in values.Entries) fn(e.Key)(e.Value); return default; }
    public static OrderedMap<K,V> MapOfArray<K,V>((K,V)[] values) { var result=new OrderedMap<K,V>(); foreach(var e in values) result=result.Add(e.Item1,e.Item2); return result; }
    public static Option<string[]> MatchGroups(string pattern, string value)
    {
        try { var match=System.Text.RegularExpressions.Regex.Match(value,pattern); return match.Success ? Option<string[]>.Some(match.Groups.Cast<System.Text.RegularExpressions.Group>().Skip(1).Select(g=>g.Value).ToArray()) : default; } catch { return default; }
    }
    public static Option<string[]> QueryXml(string query, string text) => Try(() =>
    {
        query=query.Trim();
        bool descendants=query.StartsWith("//",StringComparison.Ordinal);
        string body=descendants ? query[2..] : query;
        int slash=body.LastIndexOf('/');
        if(slash<=0 || slash==body.Length-1) throw new InvalidOperationException("Invalid XML query.");
        string selector=body[(slash+1)..];
        bool Valid(string name)=>!string.IsNullOrWhiteSpace(name) && !name.Contains(':') && name!="." && name!=".." && name!="*";
        bool attribute=selector.StartsWith('@');
        if(attribute ? !Valid(selector[1..]) : selector!="text()") throw new InvalidOperationException("Invalid XML selector.");
        string[] steps=body[..slash].Split('/',StringSplitOptions.RemoveEmptyEntries);
        if(steps.Length==0 || steps.Any(step=>!Valid(step))) throw new InvalidOperationException("Invalid XML path.");
        var root=System.Xml.Linq.XDocument.Parse(text).Root;
        if(root is null) return Array.Empty<string>();
        var elements=descendants ? root.DescendantsAndSelf(System.Xml.Linq.XName.Get(steps[0])) : root.Name.LocalName==steps[0] ? new[]{root} : root.Elements(System.Xml.Linq.XName.Get(steps[0]));
        foreach(string step in steps.Skip(1)) elements=elements.SelectMany(element=>element.Elements(System.Xml.Linq.XName.Get(step)));
        return attribute ? elements.Select(element=>element.Attribute(System.Xml.Linq.XName.Get(selector[1..]))).Where(value=>value is not null).Select(value=>value!.Value).ToArray() : elements.Select(element=>element.Value.Trim()).Where(value=>value.Length!=0).ToArray();
    });
    public static Option<T> ParseJson<T>(string text, Func<JsonElement,T> decode) => Try(() => { using var document=JsonDocument.Parse(text); return decode(document.RootElement); });
    public static Option<string[]> Files(string directory, string glob, bool recursive) => Try(() =>
    {
        directory=Path.GetFullPath(directory);
        var escaped=System.Text.RegularExpressions.Regex.Escape(glob.Replace('\\','/'));
        var pattern=escaped.Replace("\\*\\*/","___DOUBLESTAR_SLASH___").Replace("\\*\\*","___DOUBLESTAR___").Replace("\\*","[^/]*").Replace("___DOUBLESTAR_SLASH___","(?:.*/)?").Replace("___DOUBLESTAR___",".*").Replace("\\?",".");
        var regex=new System.Text.RegularExpressions.Regex("^"+pattern+"$");
        bool descend=recursive || glob.Contains('/') || glob.Contains('\\');
        IEnumerable<string> Walk(string parent)
        {
            string[] files;
            string[] children;
            try { files=Directory.GetFiles(parent); children=descend ? Directory.GetDirectories(parent) : Array.Empty<string>(); }
            catch { yield break; }
            foreach(string path in files) yield return path;
            foreach(string child in children)
                if((File.GetAttributes(child) & FileAttributes.ReparsePoint)==0)
                    foreach(string path in Walk(child)) yield return path;
        }
        if(!Directory.Exists(directory)) throw new DirectoryNotFoundException(directory);
        return Walk(directory).Where(path=>regex.IsMatch(Path.GetRelativePath(directory,path).Replace('\\','/'))).Select(path=>Path.GetRelativePath(Directory.GetCurrentDirectory(),path).Replace('\\','/')).ToArray();
    });
    public static string FloatText(double value)
    {
        if(double.IsNaN(value)) return "nan";
        if(double.IsPositiveInfinity(value)) return "infinity";
        if(double.IsNegativeInfinity(value)) return "-infinity";
        return value.ToString("G",CultureInfo.InvariantCulture);
    }
}

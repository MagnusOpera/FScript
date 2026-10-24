// Generated FScript support code. Uses only the .NET base class libraries.
using System;
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
    public static Option<long> IndexOf(string needle, string haystack) { long index = haystack.IndexOf(needle, StringComparison.Ordinal); return index < 0 ? default : Option<long>.Some(index); }
    public static Option<string> Substring(long start, long length, string value) => start < 0 || length < 0 || start > value.Length || length > value.Length-start ? default : Option<string>.Some(value.Substring((int)start,(int)length));
    public static S MapFold<K,V,S>(Func<S,Func<K,Func<V,S>>> fn, S state, OrderedMap<K,V> values) { foreach(var e in values.Entries) state=fn(state)(e.Key)(e.Value); return state; }
    public static Unit MapIter<K,V>(Func<K,Func<V,Unit>> fn, OrderedMap<K,V> values) { foreach(var e in values.Entries) fn(e.Key)(e.Value); return default; }
    public static OrderedMap<K,V> MapOfArray<K,V>((K,V)[] values) { var result=new OrderedMap<K,V>(); foreach(var e in values) result=result.Add(e.Item1,e.Item2); return result; }
    public static Option<string[]> MatchGroups(string pattern, string value)
    {
        try { var match=System.Text.RegularExpressions.Regex.Match(value,pattern); return match.Success ? Option<string[]>.Some(match.Groups.Cast<System.Text.RegularExpressions.Group>().Skip(1).Select(g=>g.Value).ToArray()) : default; } catch { return default; }
    }
    public static Option<string[]> QueryXml(string xpath, string text) => Try(() => System.Xml.XPath.Extensions.XPathSelectElements(System.Xml.Linq.XDocument.Parse(text),xpath).Select(e=>e.Value).ToArray());
    public static string FloatText(double value)
    {
        if(double.IsNaN(value)) return "nan";
        if(double.IsPositiveInfinity(value)) return "infinity";
        if(double.IsNegativeInfinity(value)) return "-infinity";
        return value.ToString("G",CultureInfo.InvariantCulture);
    }
}

// Pure support for generated programs. Keep contracts aligned with SupportLibrary/PureExterns.
using System;
using System.Linq;
using System.Collections.Generic;
using System.Text;
using System.Security.Cryptography;

namespace __NAMESPACE__;

internal static class FsPure
{
    private static bool WhiteSpace(char c) => (c>='\u0009' && c<='\u000d') || c=='\u0020' || c=='\u0085' || c=='\u00a0' || c=='\u1680' || (c>='\u2000' && c<='\u200a') || c=='\u2028' || c=='\u2029' || c=='\u202f' || c=='\u205f' || c=='\u3000';
    public static bool IsWhiteSpace(string text) => text.All(WhiteSpace);
    public static string Trim(string text,bool left,bool right)
    {
        int start=0,end=text.Length;
        if(left) while(start<end && WhiteSpace(text[start])) start++;
        if(right) while(end>start && WhiteSpace(text[end-1])) end--;
        return text.Substring(start,end-start);
    }
    public static int Count(long count) => count >= 0 && count <= int.MaxValue ? (int)count : throw new ArgumentException("Count must be between 0 and 2147483647.");
    public static long Clamp(long low,long high,long value) => low <= high ? Math.Max(low,Math.Min(high,value)) : throw new ArgumentException("Minimum exceeds maximum.");
    public static double Clamp(double low,double high,double value) => low <= high ? Math.Max(low,Math.Min(high,value)) : throw new ArgumentException("Invalid clamp bounds.");
    public static Option<long> ToInt(double value) => double.IsNaN(value) || value < -9223372036854775808.0 || value >= 9223372036854775808.0 ? default : Option<long>.Some((long)value);
    public static string[] SplitLines(string text) => text.Replace("\r\n","\n").Replace("\r","\n").Split('\n');
    public static string Repeat(long count,string value) => string.Concat(Enumerable.Repeat(value,Count(count)));
    public static T[] Take<T>(long count,IEnumerable<T> source)
    {
        var values=source.ToArray(); int n=Count(count);
        if(n>values.Length) throw new ArgumentException("Count exceeds list length.");
        return values.Take(n).ToArray();
    }
    public static T[] Skip<T>(long count,IEnumerable<T> source)
    {
        var values=source.ToArray(); int n=Count(count);
        if(n>values.Length) throw new ArgumentException("Count exceeds list length.");
        return values.Skip(n).ToArray();
    }
    public static (T[] Yes,T[] No) Partition<T>(Func<T,bool> predicate,IEnumerable<T> source)
    {
        var yes=new List<T>(); var no=new List<T>();
        foreach(var item in source) (predicate(item) ? yes : no).Add(item);
        return (yes.ToArray(),no.ToArray());
    }
    public static (A Left,B Right)[] Zip<A,B>(IEnumerable<A> left,IEnumerable<B> right)
    {
        var a=left.ToArray(); var b=right.ToArray();
        if(a.Length!=b.Length) throw new ArgumentException("Zip requires equal list lengths.");
        return a.Zip(b,(x,y)=>(x,y)).ToArray();
    }
    public static T[][] Chunk<T>(long size,IEnumerable<T> source)
    {
        int n=Count(size); if(n==0) throw new ArgumentException("Chunk size must be positive.");
        return source.Chunk(n).ToArray();
    }
    public static S[] Scan<S,T>(Func<S,Func<T,S>> folder,S initial,IEnumerable<T> source)
    {
        var result=new List<S>{initial}; var state=initial;
        foreach(var item in source) { state=folder(state)(item); result.Add(state); }
        return result.ToArray();
    }
    // Same bottom-up stable merge order as the evaluator, including callback calls.
    public static T[] SortWith<T>(Func<T,Func<T,long>> comparer,IEnumerable<T> source)
    {
        var runs=source.Select(item=>new[]{item}).ToList();
        while(runs.Count>1)
        {
            var next=new List<T[]>();
            for(int index=0;index<runs.Count;index+=2)
            {
                if(index+1==runs.Count) { next.Add(runs[index]); continue; }
                var a=runs[index]; var b=runs[index+1]; var merged=new T[a.Length+b.Length];
                int i=0,j=0,k=0;
                while(i<a.Length && j<b.Length) merged[k++]=comparer(a[i])(b[j])<=0 ? a[i++] : b[j++];
                while(i<a.Length) merged[k++]=a[i++];
                while(j<b.Length) merged[k++]=b[j++];
                next.Add(merged);
            }
            runs=next;
        }
        return runs.Count==0 ? Array.Empty<T>() : runs[0];
    }
    private static long CompareScalar<K>(K left,K right) => (left,right) switch
    {
        (string a,string b)=>Math.Sign(string.CompareOrdinal(a,b)),
        (long a,long b)=>a.CompareTo(b), (double a,double b)=>a.CompareTo(b),
        (bool a,bool b)=>a.CompareTo(b), (Unit,Unit)=>0,
        _=>throw new ArgumentException("List.sortBy requires scalar keys; use List.sortWith for composite keys.")
    };
    public static T[] SortBy<T,K>(Func<T,K> projection,IEnumerable<T> source)
    {
        var decorated=source.Select(value=>{ var key=projection(value); CompareScalar(key,key); return (Key:key,Value:value); }).ToArray();
        return SortWith<(K Key,T Value)>(a=>b=>CompareScalar(a.Key,b.Key),decorated).Select(pair=>pair.Value).ToArray();
    }
    public static (K Key,T[] Items)[] Groups<T,K>(Func<T,K> projection,IEnumerable<T> source,Func<K,K,bool> equal)
    {
        var groups=new List<(K Key,List<T> Items)>();
        foreach(var value in source)
        {
            var key=projection(value); int found=groups.FindIndex(group=>equal(key,group.Key));
            if(found<0) groups.Add((key,new List<T>{value})); else groups[found].Items.Add(value);
        }
        return groups.Select(group=>(group.Key,group.Items.ToArray())).ToArray();
    }
    public static OrderedMap<K,V> Update<K,V>(K key,Func<Option<V>,Option<V>> updater,OrderedMap<K,V> source)
    {
        var next=updater(source.Find(key)); return next.HasValue ? source.Add(key,next.Value) : source.Remove(key);
    }
    public static OrderedMap<K,V> Merge<K,V>(Func<K,Func<V,Func<V,V>>> resolver,OrderedMap<K,V> left,OrderedMap<K,V> right)
    {
        var result=left;
        foreach(var entry in right.Entries)
        {
            var old=result.Find(entry.Key);
            result=result.Add(entry.Key,old.HasValue ? resolver(entry.Key)(old.Value)(entry.Value) : entry.Value);
        }
        return result;
    }
    public static string NormalizePath(string path)
    {
        bool absolute=path.StartsWith("/",StringComparison.Ordinal); var parts=new List<string>();
        foreach(var part in path.Split('/'))
        {
            if(part=="" || part==".") continue;
            if(part==".." && parts.Count>0 && parts[^1]!="..") parts.RemoveAt(parts.Count-1);
            else if(part!=".." || !absolute) parts.Add(part);
        }
        string text=string.Join("/",parts);
        return absolute ? "/"+text : text=="" ? "." : text;
    }
    public static string CombinePath(string left,string right) => NormalizePath(right.StartsWith("/",StringComparison.Ordinal) ? right : left=="" ? right : left+"/"+right);
    public static string FileName(string path) { path=path.TrimEnd('/'); return path[(path.LastIndexOf('/')+1)..]; }
    public static Option<string> Extension(string path)
    {
        string name=FileName(path); int dot=name.LastIndexOf('.');
        return dot<=0 || dot==name.Length-1 ? default : Option<string>.Some(name[dot..]);
    }
    public static string Stem(string path) { string name=FileName(path); int dot=name.LastIndexOf('.'); return dot<=0 ? name : name[..dot]; }
    public static Option<string> Parent(string path)
    {
        path=NormalizePath(path); int slash=path.LastIndexOf('/');
        return path=="/" || path=="." ? default : Option<string>.Some(slash<0 ? "." : slash==0 ? "/" : path[..slash]);
    }
    public static Option<string> RelativePath(string basis,string target)
    {
        basis=NormalizePath(basis); target=NormalizePath(target);
        if(basis.StartsWith("/",StringComparison.Ordinal)!=target.StartsWith("/",StringComparison.Ordinal)) return default;
        var left=basis.Split('/').Where(part=>part!="" && part!=".").ToArray();
        var right=target.Split('/').Where(part=>part!="" && part!=".").ToArray();
        int common=0; while(common<left.Length && common<right.Length && left[common]==right[common]) common++;
        if(left.Skip(common).Contains("..")) return default;
        return Option<string>.Some(NormalizePath(string.Join("/",left.Skip(common).Select(_=>"..").Concat(right.Skip(common)))));
    }
    private static bool Segment(string pattern,string text)
    {
        var previous=new bool[text.Length+1]; previous[0]=true;
        foreach(char token in pattern)
        {
            var current=new bool[text.Length+1]; if(token=='*') current[0]=previous[0];
            for(int i=1;i<=text.Length;i++) current[i]=token=='*' ? previous[i] || current[i-1] : previous[i-1] && (token=='?' || token==text[i-1]);
            previous=current;
        }
        return previous[text.Length];
    }
    public static bool Glob(string pattern,string path)
    {
        var tokens=pattern.Split('/'); var parts=path.Split('/');
        if(tokens.Any(token=>token.Contains("**",StringComparison.Ordinal) && token!="**")) throw new ArgumentException("Glob ** must occupy an entire path segment.");
        var previous=new bool[parts.Length+1]; previous[0]=true;
        foreach(string token in tokens)
        {
            var current=new bool[parts.Length+1]; if(token=="**") current[0]=previous[0];
            for(int i=1;i<=parts.Length;i++) current[i]=token=="**" ? previous[i] || (current[i-1] && parts[i-1]!="") : previous[i-1] && Segment(token,parts[i-1]);
            previous=current;
        }
        return previous[parts.Length];
    }
    public static string[] GlobFilter(string pattern,IEnumerable<string> paths)
    {
        Glob(pattern,""); // Validate the pattern even when there are no inputs.
        return paths.Where(path=>Glob(pattern,path)).ToArray();
    }
    private static bool Digits(string text) => text.Length>0 && text.All(c=>c>='0' && c<='9');
    private static bool Numeric(string text) => Digits(text) && (text=="0" || text[0]!='0');
    private static bool Identifiers(string text,bool leadingZeros) => text.Split('.').All(part=>part.Length>0 && part.All(c=>(c>='0' && c<='9') || (c>='a' && c<='z') || (c>='A' && c<='Z') || c=='-') && (leadingZeros || !Digits(part) || Numeric(part)));
    public static string[]? ParseVersion(string text)
    {
        int plus=text.IndexOf('+'); string build=plus<0 ? "" : text[(plus+1)..]; string withoutBuild=plus<0 ? text : text[..plus];
        int dash=withoutBuild.IndexOf('-'); string pre=dash<0 ? "" : withoutBuild[(dash+1)..]; string core=dash<0 ? withoutBuild : withoutBuild[..dash];
        var numbers=core.Split('.');
        return numbers.Length!=3 || !numbers.All(Numeric) || (plus>=0 && !Identifiers(build,true)) || (dash>=0 && !Identifiers(pre,false)) ? null : new[]{numbers[0],numbers[1],numbers[2],pre,build};
    }
    public static string FormatVersion(string[] parts)
    {
        string text=string.Join(".",parts.Take(3))+(parts[3]=="" ? "" : "-"+parts[3])+(parts[4]=="" ? "" : "+"+parts[4]);
        return ParseVersion(text) is null ? throw new ArgumentException("Invalid semantic version components.") : text;
    }
    private static int CompareNumber(string a,string b) => a.Length==b.Length ? Math.Sign(string.CompareOrdinal(a,b)) : a.Length.CompareTo(b.Length);
    public static Option<long> CompareVersions(string left,string right)
    {
        var a=ParseVersion(left); var b=ParseVersion(right); if(a is null || b is null) return default;
        for(int i=0;i<3;i++) { int order=CompareNumber(a[i],b[i]); if(order!=0) return Option<long>.Some(order); }
        if(a[3]==b[3]) return Option<long>.Some(0);
        if(a[3]=="") return Option<long>.Some(1); if(b[3]=="") return Option<long>.Some(-1);
        var ap=a[3].Split('.'); var bp=b[3].Split('.');
        for(int i=0;i<Math.Min(ap.Length,bp.Length);i++)
        {
            bool an=Digits(ap[i]),bn=Digits(bp[i]);
            int order=an && bn ? CompareNumber(ap[i],bp[i]) : an ? -1 : bn ? 1 : Math.Sign(string.CompareOrdinal(ap[i],bp[i]));
            if(order!=0) return Option<long>.Some(order);
        }
        return Option<long>.Some(ap.Length.CompareTo(bp.Length));
    }
    private static readonly UTF8Encoding Utf8=new(false,true);
    private static Option<T> Try<T>(Func<T> operation)
    {
        try { return Option<T>.Some(operation()); }
        catch(ArgumentException) { return default; } catch(FormatException) { return default; }
    }
    private static byte[] Bytes(IEnumerable<long> values) => values.Select(value=>value>=0 && value<=255 ? (byte)value : throw new ArgumentException("Invalid byte.")).ToArray();
    private static long[] ByteList(byte[] values) => values.Select(value=>(long)value).ToArray();
    public static Option<long[]> Utf8Encode(string text) => Try(()=>ByteList(Utf8.GetBytes(text)));
    public static Option<string> Utf8Decode(IEnumerable<long> values) => Try(()=>Utf8.GetString(Bytes(values)));
    public static Option<string> Base64Encode(IEnumerable<long> values) => Try(()=>Convert.ToBase64String(Bytes(values)));
    public static Option<long[]> Base64Decode(string text) => Try(()=>ByteList(Convert.FromBase64String(text)));
    public static Option<string> HexEncode(IEnumerable<long> values) => Try(()=>Convert.ToHexString(Bytes(values)).ToLowerInvariant());
    public static Option<long[]> HexDecode(string text) => Try(()=>ByteList(Convert.FromHexString(text)));
    public static Option<string> UriEncode(string text) => Try(()=>{ Utf8.GetBytes(text); return Uri.EscapeDataString(text); });
    private static int HexDigit(byte c) => c>='0' && c<='9' ? c-'0' : c>='a' && c<='f' ? c-'a'+10 : c>='A' && c<='F' ? c-'A'+10 : throw new ArgumentException("Invalid hexadecimal digit.");
    public static Option<string> UriDecode(string text) => Try(()=>
    {
        var input=Utf8.GetBytes(text); var output=new List<byte>();
        for(int i=0;i<input.Length;i++)
        {
            if(input[i]!='%') output.Add(input[i]);
            else
            {
                if(i+2>=input.Length) throw new ArgumentException("Incomplete URI escape.");
                output.Add((byte)(16*HexDigit(input[i+1])+HexDigit(input[i+2]))); i+=2;
            }
        }
        return Utf8.GetString(output.ToArray());
    });
    public static Option<string> Sha256(string text) => Try(()=>Convert.ToHexString(SHA256.HashData(Utf8.GetBytes(text))).ToLowerInvariant());
    public static Option<string> Sha256Bytes(IEnumerable<long> values) => Try(()=>Convert.ToHexString(SHA256.HashData(Bytes(values))).ToLowerInvariant());
}

// Private native-call representation; public FScript list boundaries remain arrays.
using System;
using System.Collections;
using System.Collections.Generic;

namespace __NAMESPACE__;

internal readonly struct ListView<T> : IReadOnlyList<T>, ICollection<T>
{
    private readonly T[]? _buffer;
    private readonly int _offset;
    public int Length { get; }
    public int Count => Length;
    public long LongLength => Length;
    public ListView(T[] buffer) { _buffer=buffer; _offset=0; Length=buffer.Length; }
    private ListView(T[] buffer, int offset, int length) { _buffer=buffer; _offset=offset; Length=length; }
    public T this[int index] => this[(long)index];
    public T this[long index] => index>=0 && index<Length ? _buffer![_offset+(int)index] : throw new IndexOutOfRangeException();
    internal T[] Buffer => _buffer ?? Array.Empty<T>();
    internal int Offset => _offset;
    public ListView<T> Tail => Length==0 ? throw new InvalidOperationException("List.tail expects a non-empty list") : new(_buffer!,_offset+1,Length-1);
    public Enumerator GetEnumerator() => new(_buffer ?? Array.Empty<T>(),_offset,Length);
    IEnumerator<T> IEnumerable<T>.GetEnumerator() => GetEnumerator();
    IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
    public struct Enumerator : IEnumerator<T>
    {
        private readonly T[] _buffer;
        private readonly int _end;
        private int _index;
        internal Enumerator(T[] buffer,int offset,int length) { _buffer=buffer;_end=offset+length;_index=offset-1; }
        public T Current => _buffer[_index];
        object IEnumerator.Current => Current!;
        public bool MoveNext() { if(_index<_end) _index++;return _index<_end; }
        public void Dispose() { }
        void IEnumerator.Reset() => throw new NotSupportedException();
    }
    public void CopyTo(T[] destination,int index) => _buffer.AsSpan(_offset,Length).CopyTo(destination.AsSpan(index));
    bool ICollection<T>.IsReadOnly => true;
    bool ICollection<T>.Contains(T value)
    {
        foreach(var item in this) if(EqualityComparer<T>.Default.Equals(item,value)) return true;
        return false;
    }
    void ICollection<T>.Add(T _) => throw new NotSupportedException();
    bool ICollection<T>.Remove(T _) => throw new NotSupportedException();
    void ICollection<T>.Clear() => throw new NotSupportedException();
    public T[] ToArray()
    {
        if(Length==0) return Array.Empty<T>();
        var buffer=_buffer!;
        return _offset==0 && Length==buffer.Length ? buffer : buffer.AsSpan(_offset,Length).ToArray();
    }
    public static implicit operator ListView<T>(T[] values) => new(values);
    public static implicit operator T[](ListView<T> values) => values.ToArray();
}

internal static partial class FsRuntime
{
    internal static T[] ListBuffer<T>(ListView<T> values) => values.Buffer;
    internal static int ListOffset<T>(ListView<T> values) => values.Offset;
    // Compact selection masks preserve predicate order and avoid growing element buffers.
    public static T[] Filter<T>(Func<T,bool> predicate,ListView<T> source)
    {
        ReadOnlySpan<T> items=source.Buffer.AsSpan(source.Offset,source.Length);
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
    public static ListView<T> AppendView<T>(ListView<T> left,ListView<T> right)
    {
        if(left.Length==0) return right;
        if(right.Length==0) return left;
        var result=new T[checked(left.Length+right.Length)];
        left.CopyTo(result,0);right.CopyTo(result,left.Length);
        return result;
    }
    public static ListView<T> ConsView<T>(T head,ListView<T> tail)
    {
        var result=new T[checked(tail.Length+1)];
        result[0]=head;tail.CopyTo(result,1);
        return result;
    }
    public static ListView<T> AppendItemView<T>(ListView<T> values,T item)
    {
        var result=new T[checked(values.Length+1)];
        values.CopyTo(result,0);result[values.Length]=item;
        return result;
    }
    public static ListView<T> TailView<T>(T[] values) => new ListView<T>(values).Tail;
    public static ListView<T> TailView<T>(ListView<T> values) => values.Tail;
    public static Option<T> At<T>(long index,ListView<T> values) => index>=0 && index<values.Length ? Option<T>.Some(values[index]) : default;
    public static Unit Iter<T>(Func<T,Unit> fn,ListView<T> values) { foreach(var value in values) fn(value); return default; }
    public static S Fold<T,S>(Func<S,Func<T,S>> fn,S state,ListView<T> values) { foreach(var value in values) state=fn(state)(value); return state; }
    public static T[] Distinct<T>(ListView<T> values,Func<T,T,bool> equal)
    {
        var result=new List<T>();
        foreach(var value in values)
        {
            bool exists=false;
            foreach(var item in result) if(equal(item,value)) { exists=true; break; }
            if(!exists) result.Add(value);
        }
        return result.ToArray();
    }
    public static Option<T> Find<T>(Func<T,bool> fn,ListView<T> values)
    {
        foreach(var value in values) if(fn(value)) return Option<T>.Some(value);
        return default;
    }
    public static Option<long> FindIndex<T>(Func<T,bool> fn,ListView<T> values)
    {
        for(int index=0;index<values.Length;index++) if(fn(values[index])) return Option<long>.Some(index);
        return default;
    }
}

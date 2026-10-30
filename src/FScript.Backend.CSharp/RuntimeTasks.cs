// Generated task support. Callbacks return ordinary values; nested tasks stay nested.
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace __NAMESPACE__;

internal static class FsTasks
{
    private sealed class Scope
    {
        public readonly object Gate=new();
        public readonly Dictionary<Task,bool> Pending=new(ReferenceEqualityComparer.Instance);
        public readonly Dictionary<Task,Task> Aliases=new(ReferenceEqualityComparer.Instance);
        public bool Active=true;
        public void CheckFailures()
        {
            Task[] tasks;
            lock(Gate) tasks=Pending.Keys.Concat(Aliases.Keys).ToArray();
            foreach(var task in tasks) if(task.IsFaulted) task.GetAwaiter().GetResult();
        }
        public void Complete()
        {
            CheckFailures();
            int count;
            lock(Gate) count=Pending.Count(pair=>!pair.Value);
            if(count!=0) throw new InvalidOperationException($"Program completed with {count} unawaited task(s)");
        }
    }
    private static readonly AsyncLocal<Scope?> Current=new();
    private static T Track<T>(T task) where T:Task
    {
        var scope=Current.Value;
        if(scope is not null) lock(scope.Gate) scope.Pending.TryAdd(task,false);
        return task;
    }
    private static void Observe(Task task)
    {
        var scope=Current.Value;
        if(scope is not null) lock(scope.Gate)
        {
            while(scope.Aliases.TryGetValue(task,out var source)) task=source;
            if(scope.Pending.ContainsKey(task)) scope.Pending[task]=true;
        }
    }
    private static void Check() => Current.Value?.CheckFailures();
    public static T Run<T>(Func<T> body)
    {
        var previous=Current.Value;
        if(previous is {Active:true}) return body();
        var scope=new Scope(); Current.Value=scope;
        try { var result=body(); scope.Complete(); return result; }
        finally { scope.Active=false; Current.Value=previous; }
    }
    public static Task<T> Spawn<T>(Func<Unit,T> callback)
    {
        Check();
        // StartNew has no automatic Task<Task<T>> unwrapping overload.
        return Track(Task.Factory.StartNew(()=>{ Check(); return callback(default); },CancellationToken.None,TaskCreationOptions.DenyChildAttach,TaskScheduler.Default));
    }
    public static Task<T> FromResult<T>(T value)
    {
        Check(); var completion=new TaskCompletionSource<T>(TaskCreationOptions.RunContinuationsAsynchronously);
        completion.SetResult(value); return Track(completion.Task);
    }
    private static Task<Unit> DelayCore(int milliseconds)
    {
        var completion=new TaskCompletionSource<Unit>(TaskCreationOptions.RunContinuationsAsynchronously);
        Task.Delay(milliseconds).ContinueWith(_=>completion.SetResult(default),CancellationToken.None,TaskContinuationOptions.ExecuteSynchronously,TaskScheduler.Default);
        return completion.Task;
    }
    public static Task<Unit> Delay(long milliseconds)
    {
        if(milliseconds<0 || milliseconds>int.MaxValue) throw new ArgumentException("Task.delay expects milliseconds in 0..2147483647.");
        Check(); return Track(DelayCore((int)milliseconds));
    }
    public static T Await<T>(Task<T> task)
    {
        Observe(task); var result=task.GetAwaiter().GetResult(); Check(); return result;
    }
    // SDK transport is a representation conversion, not a user continuation.
    // Preserve completed status so conversions cannot change WhenAny tie behavior.
    public static Task<R> Convert<T,R>(Task<T> task,Func<T,R> conversion)
    {
        var converted=task.ContinueWith(completed=>conversion(completed.GetAwaiter().GetResult()),CancellationToken.None,TaskContinuationOptions.ExecuteSynchronously,TaskScheduler.Default);
        var scope=Current.Value;
        if(scope is not null) lock(scope.Gate) scope.Aliases.Add(converted,task);
        return converted;
    }
    public static Task<R> Continue<T,R>(Task<T> task,Func<T,R> callback)
    {
        Check(); Observe(task);
        return Track(Task.Run<R>(async ()=>
        {
            var value=await task.ConfigureAwait(false); Check(); return callback(value);
        }));
    }
    public static Task<R> WhenAll<T,R>(IEnumerable<Task<T>> tasks,Func<T[],R> callback)
    {
        Check(); var inputs=tasks.ToArray(); foreach(var task in inputs) Observe(task);
        var all=Task.WhenAll(inputs);
        return Track(Task.Run<R>(async ()=>
        {
            var values=await all.ConfigureAwait(false); Check(); return callback(values);
        }));
    }
    public static Task<R> WhenAny<T,R>(IEnumerable<Task<T>> tasks,Func<T,R> callback)
    {
        var inputs=tasks.ToArray();
        if(inputs.Length==0) throw new ArgumentException("Task.whenAny expects a non-empty task list.");
        Check();
        // Register the race now, before the callback worker is scheduled.
        var any=Task.WhenAny(inputs);
        return Track(Task.Run<R>(async ()=>
        {
            var winner=await any.ConfigureAwait(false); Observe(winner);
            var value=await winner.ConfigureAwait(false); Check(); return callback(value);
        }));
    }
    public static T[] AwaitAll<T>(IEnumerable<Task<T>> tasks)
    {
        var inputs=tasks.ToArray(); foreach(var task in inputs) Observe(task);
        var values=Task.WhenAll(inputs).GetAwaiter().GetResult(); Check(); return values;
    }
}

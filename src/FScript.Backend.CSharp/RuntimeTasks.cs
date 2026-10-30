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
        public void CheckActive()
        {
            lock(Gate) if(!Active) throw new InvalidOperationException("Script task scope has ended");
        }
        public Exception[] Errors(Exception? foreground)
        {
            var errors=new List<Exception>();
            void Add(Exception error)
            {
                if(error is AggregateException aggregate)
                    foreach(var inner in aggregate.Flatten().InnerExceptions) Add(inner);
                else if(!errors.Any(existing=>ReferenceEquals(existing,error))) errors.Add(error);
            }
            if(foreground is not null) Add(foreground);
            lock(Gate)
                foreach(var task in Pending.Keys.Concat(Aliases.Keys))
                {
                    if(task.IsFaulted) Add(task.Exception!);
                    else if(task.IsCanceled) Add(new TaskCanceledException(task));
                }
            return errors.ToArray();
        }
        public void Join()
        {
            while(true)
            {
                Task[] tasks;
                lock(Gate) tasks=Pending.Keys.Concat(Aliases.Keys).ToArray();
                try { Task.WhenAll(tasks).GetAwaiter().GetResult(); } catch { }
                lock(Gate)
                    if(tasks.Length==Pending.Count+Aliases.Count && Pending.Keys.Concat(Aliases.Keys).All(task=>task.IsCompleted)) break;
            }
        }
        public void Close() { lock(Gate) Active=false; }
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
    private static void Check() => Current.Value?.CheckActive();
    public static T Run<T>(Func<T> body)
    {
        var previous=Current.Value;
        if(previous is {Active:true}) return body();
        var scope=new Scope(); Current.Value=scope;
        try
        {
            T result=default!;
            Exception? foreground=null;
            try { result=body(); } catch(Exception error) { foreground=error; }
            scope.Join();
            var errors=scope.Errors(foreground);
            if(errors.Length==1) System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(errors[0]).Throw();
            if(errors.Length>1) throw new AggregateException("Script execution failed",errors);
            return result;
        }
        finally { scope.Close(); Current.Value=previous; }
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
    private static async Task<Unit> DelayCore(int milliseconds)
    {
        await Task.Delay(milliseconds).ConfigureAwait(false);
        return default;
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

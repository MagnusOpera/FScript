namespace FScript.Runtime.Tests

open System
open System.Diagnostics
open System.IO
open System.Text
open System.Threading
open System.Threading.Tasks

/// Portable child-program modes shared by the runtime and compiled FFI tests.
type ProcessFixture private () =
    static member Main(arguments: string array) =
        match arguments with
        | [||] -> 0
        | args when args.[0] <> "--process-fixture" -> 0
        | args ->
            let args = args.[1..]
            Console.InputEncoding <- UTF8Encoding(false)
            Console.OutputEncoding <- UTF8Encoding(false)
            Console.Out.NewLine <- "\n"
            match args.[0] with
            | "echo" ->
                for argument in args.[1..] do Console.WriteLine argument
                Console.Error.Write "error\n"
                7
            | "stdin" -> Console.Write(Console.In.ReadToEnd()); 0
            | "context" -> Console.Write(Directory.GetCurrentDirectory()+"\n"+Environment.GetEnvironmentVariable(args.[1])); 0
            | "flood" ->
                let count = Int32.Parse args.[1]
                Task.WhenAll([|Task.Run(Action(fun () -> Console.Out.Write(String('o',count))))
                               Task.Run(Action(fun () -> Console.Error.Write(String('e',count))))|]).GetAwaiter().GetResult()
                0
            | "delay" -> Thread.Sleep(Int32.Parse args.[1]); 0
            | "write" -> Thread.Sleep 100; File.WriteAllText(args.[1],"done"); 0
            | "pid-file" -> File.WriteAllText(args.[1], string Environment.ProcessId); Thread.Sleep 30000; 0
            | "pid" -> Console.WriteLine Environment.ProcessId; Console.Out.Flush(); Thread.Sleep 30000; 0
            | "invalid-utf8" ->
                let bytes = [|0xffuy; byte '\n'|]
                Console.OpenStandardOutput().Write(bytes,0,bytes.Length)
                0
            | "spawn" ->
                let start = ProcessStartInfo("dotnet", UseShellExecute=false)
                for argument in [typeof<ProcessFixture>.Assembly.Location; "--process-fixture"; "delay"; "1000"] do
                    start.ArgumentList.Add argument
                use child = Process.Start(start) |> nonNull
                0
            | _ -> 99

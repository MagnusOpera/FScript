using System.Runtime.InteropServices;
using System.Security.Cryptography;
using System.Text;

namespace FScript.Ffi.Rendering;

// Only the GLFW/OpenGL entry points used by this sample. Function pointers keep
// calls statically typed and work under Native AOT without generated delegates.
internal static unsafe class Glfw
{
    private static readonly nint library = Load();
    private static nint Export(string name) => NativeLibrary.GetExport(library, name);
    internal static readonly delegate* unmanaged[Cdecl]<int> Init = (delegate* unmanaged[Cdecl]<int>)Export("glfwInit");
    internal static readonly delegate* unmanaged[Cdecl]<void> Terminate = (delegate* unmanaged[Cdecl]<void>)Export("glfwTerminate");
    internal static readonly delegate* unmanaged[Cdecl]<int, int, void> WindowHint = (delegate* unmanaged[Cdecl]<int, int, void>)Export("glfwWindowHint");
    internal static readonly delegate* unmanaged[Cdecl]<int, int, byte*, nint, nint, nint> CreateWindow = (delegate* unmanaged[Cdecl]<int, int, byte*, nint, nint, nint>)Export("glfwCreateWindow");
    internal static readonly delegate* unmanaged[Cdecl]<nint, void> DestroyWindow = (delegate* unmanaged[Cdecl]<nint, void>)Export("glfwDestroyWindow");
    internal static readonly delegate* unmanaged[Cdecl]<nint, void> MakeContextCurrent = (delegate* unmanaged[Cdecl]<nint, void>)Export("glfwMakeContextCurrent");
    internal static readonly delegate* unmanaged[Cdecl]<int, void> SwapInterval = (delegate* unmanaged[Cdecl]<int, void>)Export("glfwSwapInterval");
    internal static readonly delegate* unmanaged[Cdecl]<nint, void> SwapBuffers = (delegate* unmanaged[Cdecl]<nint, void>)Export("glfwSwapBuffers");
    internal static readonly delegate* unmanaged[Cdecl]<void> PollEvents = (delegate* unmanaged[Cdecl]<void>)Export("glfwPollEvents");
    internal static readonly delegate* unmanaged[Cdecl]<nint, int> WindowShouldClose = (delegate* unmanaged[Cdecl]<nint, int>)Export("glfwWindowShouldClose");
    internal static readonly delegate* unmanaged[Cdecl]<nint, int, int> GetKey = (delegate* unmanaged[Cdecl]<nint, int, int>)Export("glfwGetKey");
    internal static readonly delegate* unmanaged[Cdecl]<nint, int*, int*, void> GetFramebufferSize = (delegate* unmanaged[Cdecl]<nint, int*, int*, void>)Export("glfwGetFramebufferSize");
    internal static readonly delegate* unmanaged[Cdecl]<byte*, nint> GetProcAddress = (delegate* unmanaged[Cdecl]<byte*, nint>)Export("glfwGetProcAddress");
    internal static readonly delegate* unmanaged[Cdecl]<byte**, int> GetError = (delegate* unmanaged[Cdecl]<byte**, int>)Export("glfwGetError");

    internal static string Error()
    {
        byte* description = null;
        int code = GetError(&description);
        return $"GLFW error {code}: {Marshal.PtrToStringUTF8((nint)description)}";
    }

    private static nint Load()
    {
        string os = OperatingSystem.IsMacOS() ? "osx" : OperatingSystem.IsWindows() ? "win" : "linux";
        string arch = RuntimeInformation.ProcessArchitecture.ToString().ToLowerInvariant();
        string file = OperatingSystem.IsMacOS() ? "libglfw.3.dylib" : OperatingSystem.IsWindows() ? "glfw3.dll" : "libglfw.so.3";
        string resource = $"FScript.Ffi.Rendering.runtimes.{os}-{arch}/native/{file}";
        var assembly = typeof(Glfw).Assembly;
        string? storedName = assembly.GetManifestResourceNames().FirstOrDefault(name => name.Replace('\\', '/') == resource);
        using Stream input = storedName is null ? throw new PlatformNotSupportedException($"No GLFW runtime for {os}-{arch}.")
            : assembly.GetManifestResourceStream(storedName)!;
        using var bytes = new MemoryStream();
        input.CopyTo(bytes);
        byte[] content = bytes.ToArray();
        string hash = Convert.ToHexString(SHA256.HashData(content));
        string directory = Path.Combine(Path.GetTempPath(), "fscript-glfw", hash);
        Directory.CreateDirectory(directory);
        string path = Path.Combine(directory, file);
        if (!File.Exists(path))
        {
            string temporary = Path.Combine(directory, Guid.NewGuid().ToString("N"));
            try
            {
                File.WriteAllBytes(temporary, content);
                try { File.Move(temporary, path); }
                catch (IOException) when (File.Exists(path)) { }
            }
            finally { if (File.Exists(temporary)) File.Delete(temporary); }
        }
        return NativeLibrary.Load(path);
    }
}

internal sealed unsafe class NativeGl
{
    private static nint Load(string name)
    {
        byte[] utf8 = Encoding.UTF8.GetBytes(name + '\0');
        fixed (byte* pointer = utf8)
        {
            nint result = Glfw.GetProcAddress(pointer);
            return result != 0 ? result : throw new InvalidOperationException($"Missing OpenGL entry point {name}.");
        }
    }
    internal readonly delegate* unmanaged<uint, void> Enable = (delegate* unmanaged<uint, void>)Load("glEnable");
    internal readonly delegate* unmanaged<uint, void> Disable = (delegate* unmanaged<uint, void>)Load("glDisable");
    internal readonly delegate* unmanaged<int, int, int, int, void> Viewport = (delegate* unmanaged<int, int, int, int, void>)Load("glViewport");
    internal readonly delegate* unmanaged<float, float, float, float, void> ClearColor = (delegate* unmanaged<float, float, float, float, void>)Load("glClearColor");
    internal readonly delegate* unmanaged<uint, void> Clear = (delegate* unmanaged<uint, void>)Load("glClear");
    internal readonly delegate* unmanaged<int, uint*, void> GenVertexArrays = (delegate* unmanaged<int, uint*, void>)Load("glGenVertexArrays");
    internal readonly delegate* unmanaged<int, uint*, void> GenBuffers = (delegate* unmanaged<int, uint*, void>)Load("glGenBuffers");
    internal readonly delegate* unmanaged<uint, void> BindVertexArray = (delegate* unmanaged<uint, void>)Load("glBindVertexArray");
    internal readonly delegate* unmanaged<uint, uint, void> BindBuffer = (delegate* unmanaged<uint, uint, void>)Load("glBindBuffer");
    internal readonly delegate* unmanaged<uint, nint, void*, uint, void> BufferData = (delegate* unmanaged<uint, nint, void*, uint, void>)Load("glBufferData");
    internal readonly delegate* unmanaged<uint, int, uint, byte, int, void*, void> VertexAttribPointer = (delegate* unmanaged<uint, int, uint, byte, int, void*, void>)Load("glVertexAttribPointer");
    internal readonly delegate* unmanaged<uint, void> EnableVertexAttribArray = (delegate* unmanaged<uint, void>)Load("glEnableVertexAttribArray");
    internal readonly delegate* unmanaged<uint, void> UseProgram = (delegate* unmanaged<uint, void>)Load("glUseProgram");
    internal readonly delegate* unmanaged<uint, byte*, int> GetUniformLocation = (delegate* unmanaged<uint, byte*, int>)Load("glGetUniformLocation");
    internal readonly delegate* unmanaged<int, int, byte, float*, void> UniformMatrix4fv = (delegate* unmanaged<int, int, byte, float*, void>)Load("glUniformMatrix4fv");
    internal readonly delegate* unmanaged<int, float, float, float, void> Uniform3f = (delegate* unmanaged<int, float, float, float, void>)Load("glUniform3f");
    internal readonly delegate* unmanaged<int, float, void> Uniform1f = (delegate* unmanaged<int, float, void>)Load("glUniform1f");
    internal readonly delegate* unmanaged<uint, int, uint, void*, void> DrawElements = (delegate* unmanaged<uint, int, uint, void*, void>)Load("glDrawElements");
    internal readonly delegate* unmanaged<int, int, int, int, uint, uint, void*, void> ReadPixels = (delegate* unmanaged<int, int, int, int, uint, uint, void*, void>)Load("glReadPixels");
    internal readonly delegate* unmanaged<uint, uint> CreateShader = (delegate* unmanaged<uint, uint>)Load("glCreateShader");
    internal readonly delegate* unmanaged<uint, int, byte**, int*, void> ShaderSource = (delegate* unmanaged<uint, int, byte**, int*, void>)Load("glShaderSource");
    internal readonly delegate* unmanaged<uint, void> CompileShader = (delegate* unmanaged<uint, void>)Load("glCompileShader");
    internal readonly delegate* unmanaged<uint, uint, int*, void> GetShaderiv = (delegate* unmanaged<uint, uint, int*, void>)Load("glGetShaderiv");
    internal readonly delegate* unmanaged<uint, int, int*, byte*, void> GetShaderInfoLog = (delegate* unmanaged<uint, int, int*, byte*, void>)Load("glGetShaderInfoLog");
    internal readonly delegate* unmanaged<uint> CreateProgram = (delegate* unmanaged<uint>)Load("glCreateProgram");
    internal readonly delegate* unmanaged<uint, uint, void> AttachShader = (delegate* unmanaged<uint, uint, void>)Load("glAttachShader");
    internal readonly delegate* unmanaged<uint, void> LinkProgram = (delegate* unmanaged<uint, void>)Load("glLinkProgram");
    internal readonly delegate* unmanaged<uint, uint, int*, void> GetProgramiv = (delegate* unmanaged<uint, uint, int*, void>)Load("glGetProgramiv");
    internal readonly delegate* unmanaged<uint, int, int*, byte*, void> GetProgramInfoLog = (delegate* unmanaged<uint, int, int*, byte*, void>)Load("glGetProgramInfoLog");
    internal readonly delegate* unmanaged<uint, void> DeleteShader = (delegate* unmanaged<uint, void>)Load("glDeleteShader");
    internal readonly delegate* unmanaged<uint, void> DeleteProgram = (delegate* unmanaged<uint, void>)Load("glDeleteProgram");
    internal readonly delegate* unmanaged<int, uint*, void> DeleteVertexArrays = (delegate* unmanaged<int, uint*, void>)Load("glDeleteVertexArrays");
    internal readonly delegate* unmanaged<int, uint*, void> DeleteBuffers = (delegate* unmanaged<int, uint*, void>)Load("glDeleteBuffers");
}

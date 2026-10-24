using System.Numerics;
using System.Runtime.InteropServices;
using Silk.NET.Core.Loader;
using Silk.NET.Input;
using Silk.NET.Maths;
using Silk.NET.OpenGL;
using Silk.NET.Windowing;

namespace FScript.Ffi.Rendering;

public sealed unsafe class OpenGlGraphics : IGraphics, IDisposable
{
    private readonly IWindow window;
    private GL? api;
    private IInputContext? input;
    private readonly List<(uint Vao, uint Vbo, uint Ebo, uint Count)> meshes = [];
    private uint program;
    private Matrix4x4 view = Matrix4x4.Identity;
    private float fieldOfView = MathF.PI / 4;
    private Vector3 eye;
    private Vector3 lightPosition;
    private Vector3 lightColor = Vector3.One;
    private float ambient = 0.18f;
    private readonly int thread = Environment.CurrentManagedThreadId;
    private bool disposed;
    public int FramesRendered { get; private set; }
    public int ColoredPixels { get; private set; }

    private static readonly Lazy<bool> nativeSearch = new(() =>
    {
        // A provider is loaded outside the CLI's .deps.json. Locate its bundled
        // native assets relative to this assembly, rather than the CLI installation.
        if (PathResolver.Default is DefaultPathResolver resolver)
        {
            string os = OperatingSystem.IsMacOS() ? "osx" : OperatingSystem.IsWindows() ? "win" : "linux";
            string arch = RuntimeInformation.ProcessArchitecture.ToString().ToLowerInvariant();
            string directory = Path.Combine(Path.GetDirectoryName(typeof(OpenGlGraphics).Assembly.Location)!,
                "runtimes", $"{os}-{arch}", "native");
            // Surface macOS signing/dependency errors rather than Window.Create's
            // generic "no suitable platform" message. Keep the module for this process.
            if (OperatingSystem.IsMacOS()) NativeLibrary.Load(Path.Combine(directory, "libglfw.3.dylib"));
            resolver.Resolvers.Insert(0, name => [Path.Combine(directory, Path.GetFileName(name))]);
        }
        return true;
    });

    public OpenGlGraphics()
    {
        _ = nativeSearch.Value;
        _ = Silk.NET.GLFW.GlfwProvider.GLFW.Value;
        var options = WindowOptions.Default;
        options.Title = "FScript FFI — Utah teapot";
        options.Size = new Vector2D<int>(960, 720);
        options.API = new GraphicsAPI(ContextAPI.OpenGL, ContextProfile.Core,
            ContextFlags.ForwardCompatible, new APIVersion(3, 3));
        options.VSync = true;
        window = Window.Create(options);
    }

    private GL Api
    {
        get
        {
            ObjectDisposedException.ThrowIf(disposed, this);
            if (Environment.CurrentManagedThreadId != thread)
                throw new InvalidOperationException("Graphics calls must run on the window thread.");
            return api ?? throw new InvalidOperationException("The OpenGL context is not ready.");
        }
    }

    public void Run(Action initialize, Action<double> render, int maxFrames = 0)
    {
        double elapsed = 0;
        window.Load += () =>
        {
            api = window.CreateOpenGL();
            input = window.CreateInput();
            foreach (var keyboard in input.Keyboards)
                keyboard.KeyDown += (_, key, _) => { if (key == Key.Escape) window.Close(); };
            program = LinkProgram();
            Api.Enable(EnableCap.DepthTest);
            // The teapot has open surfaces at the spout and lid; draw both sides.
            Api.Disable(EnableCap.CullFace);
            initialize();
        };
        window.Render += delta =>
        {
            if (window.FramebufferSize.X <= 0 || window.FramebufferSize.Y <= 0) return;
            elapsed += delta;
            Api.Viewport(window.FramebufferSize);
            render(elapsed);
            FramesRendered++;
            if (maxFrames > 0 && FramesRendered >= maxFrames)
            {
                // Read the final frame before buffer swap to check actual rasterization.
                var width = window.FramebufferSize.X;
                var height = window.FramebufferSize.Y;
                var pixels = new byte[width * height * 4];
                fixed (byte* pointer = pixels)
                    Api.ReadPixels(0, 0, (uint)width, (uint)height, PixelFormat.Rgba, PixelType.UnsignedByte, pointer);
                for (int i = 0; i < pixels.Length; i += 4)
                    if (pixels[i] > 70 && pixels[i] > pixels[i + 2] * 1.2) ColoredPixels++;
                window.Close();
            }
        };
        // Keep the context alive until resources are released. WindowExtensions.Run()
        // resets the native window before returning, which is too late for GL cleanup.
        try
        {
            window.Initialize();
            while (!window.IsClosing)
            {
                window.DoEvents();
                if (!window.IsClosing) window.DoUpdate();
                if (!window.IsClosing) window.DoRender();
            }
        }
        finally { Dispose(); }
    }

    public int Upload(float[] vertices, uint[] indices)
    {
        var gl = Api;
        uint vao = gl.GenVertexArray(), vbo = gl.GenBuffer(), ebo = gl.GenBuffer();
        meshes.Add((vao, vbo, ebo, (uint)indices.Length));
        gl.BindVertexArray(vao);
        gl.BindBuffer(BufferTargetARB.ArrayBuffer, vbo);
        fixed (float* data = vertices)
            gl.BufferData(BufferTargetARB.ArrayBuffer, (nuint)(vertices.Length * sizeof(float)), data, BufferUsageARB.StaticDraw);
        gl.BindBuffer(BufferTargetARB.ElementArrayBuffer, ebo);
        fixed (uint* data = indices)
            gl.BufferData(BufferTargetARB.ElementArrayBuffer, (nuint)(indices.Length * sizeof(uint)), data, BufferUsageARB.StaticDraw);
        gl.VertexAttribPointer(0, 3, VertexAttribPointerType.Float, false, 6 * sizeof(float), (void*)0);
        gl.EnableVertexAttribArray(0);
        gl.VertexAttribPointer(1, 3, VertexAttribPointerType.Float, false, 6 * sizeof(float), (void*)(3 * sizeof(float)));
        gl.EnableVertexAttribArray(1);
        return meshes.Count - 1;
    }

    public void Clear(Vector3 color)
    {
        Api.ClearColor(color.X, color.Y, color.Z, 1);
        Api.Clear(ClearBufferMask.ColorBufferBit | ClearBufferMask.DepthBufferBit);
    }
    public void Camera(Vector3 position, Vector3 target, float fov)
    {
        _ = Api;
        eye = position;
        view = Matrix4x4.CreateLookAt(position, target, Vector3.UnitY);
        fieldOfView = fov;
    }
    public void Light(Vector3 position, Vector3 color, float strength)
    {
        _ = Api;
        lightPosition = position;
        lightColor = color;
        ambient = strength;
    }
    public void Draw(int mesh, Vector3 rotation, Vector3 color, float shininess)
    {
        var gl = Api;
        if (mesh < 0 || mesh >= meshes.Count) throw new ArgumentOutOfRangeException(nameof(mesh));
        var model = Matrix4x4.CreateRotationX(rotation.X) * Matrix4x4.CreateRotationY(rotation.Y) * Matrix4x4.CreateRotationZ(rotation.Z);
        // OpenGL's clip depth is [-1,1], unlike System.Numerics' perspective helper.
        float f = 1 / MathF.Tan(fieldOfView / 2), near = 0.1f, far = 100;
        float aspect = (float)window.FramebufferSize.X / Math.Max(1, window.FramebufferSize.Y);
        var projection = new Matrix4x4(f / aspect, 0, 0, 0, 0, f, 0, 0,
            0, 0, (far + near) / (near - far), -1, 0, 0, 2 * far * near / (near - far), 0);
        var mvp = model * view * projection;
        gl.UseProgram(program);
        // Row-major System.Numerics matrices become the transposed column-major GLSL matrices.
        gl.UniformMatrix4(gl.GetUniformLocation(program, "model"), 1, false, (float*)&model);
        gl.UniformMatrix4(gl.GetUniformLocation(program, "mvp"), 1, false, (float*)&mvp);
        Uniform("eye", eye); Uniform("lightPosition", lightPosition);
        Uniform("lightColor", lightColor); Uniform("materialColor", color);
        gl.Uniform1(gl.GetUniformLocation(program, "ambient"), ambient);
        gl.Uniform1(gl.GetUniformLocation(program, "shininess"), shininess);
        gl.BindVertexArray(meshes[mesh].Vao);
        gl.DrawElements(PrimitiveType.Triangles, meshes[mesh].Count, DrawElementsType.UnsignedInt, (void*)0);
    }
    private void Uniform(string name, Vector3 value) => Api.Uniform3(Api.GetUniformLocation(program, name), value.X, value.Y, value.Z);
    private uint Shader(ShaderType type, string source)
    {
        uint shader = Api.CreateShader(type);
        Api.ShaderSource(shader, source);
        Api.CompileShader(shader);
        Api.GetShader(shader, ShaderParameterName.CompileStatus, out int status);
        if (status == 0)
        {
            var message = Api.GetShaderInfoLog(shader);
            Api.DeleteShader(shader);
            throw new InvalidOperationException($"Shader compilation failed: {message}");
        }
        return shader;
    }
    private uint LinkProgram()
    {
        uint vertex = Shader(ShaderType.VertexShader, VertexShader), fragment = 0, linked = 0;
        try
        {
            fragment = Shader(ShaderType.FragmentShader, FragmentShader);
            linked = Api.CreateProgram();
            Api.AttachShader(linked, vertex); Api.AttachShader(linked, fragment);
            Api.LinkProgram(linked);
            Api.GetProgram(linked, ProgramPropertyARB.LinkStatus, out int status);
            if (status == 0) throw new InvalidOperationException($"Shader linking failed: {Api.GetProgramInfoLog(linked)}");
            return linked;
        }
        catch { if (linked != 0) Api.DeleteProgram(linked); throw; }
        finally { Api.DeleteShader(vertex); if (fragment != 0) Api.DeleteShader(fragment); }
    }
    public void Dispose()
    {
        if (disposed) return;
        try
        {
            if (api is not null)
            {
                window.MakeCurrent();
                foreach (var mesh in meshes)
                {
                    api.DeleteVertexArray(mesh.Vao); api.DeleteBuffer(mesh.Vbo); api.DeleteBuffer(mesh.Ebo);
                }
                if (program != 0) api.DeleteProgram(program);
            }
            input?.Dispose();
            api?.Dispose();
        }
        finally { disposed = true; window.Dispose(); }
    }
    private const string VertexShader = """
        #version 330 core
        layout(location = 0) in vec3 position;
        layout(location = 1) in vec3 normal;
        uniform mat4 model;
        uniform mat4 mvp;
        out vec3 worldPosition;
        out vec3 worldNormal;
        void main() {
            worldPosition = (model * vec4(position, 1.0)).xyz;
            worldNormal = mat3(model) * normal;
            gl_Position = mvp * vec4(position, 1.0);
        }
        """;
    private const string FragmentShader = """
        #version 330 core
        in vec3 worldPosition;
        in vec3 worldNormal;
        uniform vec3 eye, lightPosition, lightColor, materialColor;
        uniform float ambient, shininess;
        out vec4 color;
        void main() {
            vec3 n = normalize(worldNormal);
            if (!gl_FrontFacing) n = -n;
            vec3 l = normalize(lightPosition - worldPosition);
            vec3 v = normalize(eye - worldPosition);
            float diffuse = max(dot(n, l), 0.0);
            float specular = diffuse > 0.0 ? pow(max(dot(n, normalize(l + v)), 0.0), shininess) : 0.0;
            color = vec4(materialColor * (ambient + diffuse * lightColor) + 0.35 * specular * lightColor, 1.0);
        }
        """;
}

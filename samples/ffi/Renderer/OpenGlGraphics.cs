using System.Numerics;
using System.Diagnostics;
using System.Text;

namespace FScript.Ffi.Rendering;

public sealed unsafe class OpenGlGraphics : IGraphics, IDisposable
{
    private readonly nint window;
    private NativeGl? api;
    private int framebufferWidth, framebufferHeight;
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

    public OpenGlGraphics()
    {
        if (Glfw.Init() == 0) throw new InvalidOperationException(Glfw.Error());
        Glfw.WindowHint(0x00022002, 3); // GLFW_CONTEXT_VERSION_MAJOR
        Glfw.WindowHint(0x00022003, 3); // GLFW_CONTEXT_VERSION_MINOR
        Glfw.WindowHint(0x00022008, 0x00032001); // GLFW_OPENGL_PROFILE = CORE
        Glfw.WindowHint(0x00022006, 1); // GLFW_OPENGL_FORWARD_COMPAT
        byte[] title = Encoding.UTF8.GetBytes("FScript FFI — Utah teapot\0");
        fixed (byte* pointer = title) window = Glfw.CreateWindow(960, 720, pointer, 0, 0);
        if (window == 0)
        {
            string error = Glfw.Error();
            Glfw.Terminate();
            throw new InvalidOperationException(error);
        }
    }

    private NativeGl Api
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
        try
        {
            Glfw.MakeContextCurrent(window);
            Glfw.SwapInterval(1);
            api = new NativeGl();
            program = LinkProgram();
            Api.Enable(0x0B71); // GL_DEPTH_TEST
            Api.Disable(0x0B44); // GL_CULL_FACE, the teapot has open surfaces.
            initialize();
            var clock = Stopwatch.StartNew();
            while (Glfw.WindowShouldClose(window) == 0)
            {
                Glfw.PollEvents();
                if (Glfw.WindowShouldClose(window) != 0 || Glfw.GetKey(window, 256) == 1) break;
                int width, height;
                Glfw.GetFramebufferSize(window, &width, &height);
                framebufferWidth = width;
                framebufferHeight = height;
                if (width <= 0 || height <= 0) { Thread.Sleep(16); continue; }
                Api.Viewport(0, 0, width, height);
                render(clock.Elapsed.TotalSeconds);
                FramesRendered++;
                if (maxFrames > 0 && FramesRendered >= maxFrames)
                {
                    var pixels = new byte[width * height * 4];
                    fixed (byte* pointer = pixels)
                        Api.ReadPixels(0, 0, width, height, 0x1908, 0x1401, pointer); // RGBA, UNSIGNED_BYTE
                    for (int i = 0; i < pixels.Length; i += 4)
                        if (pixels[i] > 70 && pixels[i] > pixels[i + 2] * 1.2) ColoredPixels++;
                    break;
                }
                Glfw.SwapBuffers(window);
            }
        }
        finally { Dispose(); }
    }

    public int Upload(float[] vertices, uint[] indices)
    {
        var gl = Api;
        uint vao, vbo, ebo;
        gl.GenVertexArrays(1, &vao); gl.GenBuffers(1, &vbo); gl.GenBuffers(1, &ebo);
        meshes.Add((vao, vbo, ebo, (uint)indices.Length));
        gl.BindVertexArray(vao);
        gl.BindBuffer(0x8892, vbo);
        fixed (float* data = vertices)
            gl.BufferData(0x8892, (nint)(vertices.Length * sizeof(float)), data, 0x88E4);
        gl.BindBuffer(0x8893, ebo);
        fixed (uint* data = indices)
            gl.BufferData(0x8893, (nint)(indices.Length * sizeof(uint)), data, 0x88E4);
        gl.VertexAttribPointer(0, 3, 0x1406, 0, 6 * sizeof(float), (void*)0);
        gl.EnableVertexAttribArray(0);
        gl.VertexAttribPointer(1, 3, 0x1406, 0, 6 * sizeof(float), (void*)(3 * sizeof(float)));
        gl.EnableVertexAttribArray(1);
        return meshes.Count - 1;
    }

    public void Clear(Vector3 color)
    {
        Api.ClearColor(color.X, color.Y, color.Z, 1);
        Api.Clear(0x4000 | 0x0100);
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
        float aspect = (float)framebufferWidth / Math.Max(1, framebufferHeight);
        var projection = new Matrix4x4(f / aspect, 0, 0, 0, 0, f, 0, 0,
            0, 0, (far + near) / (near - far), -1, 0, 0, 2 * far * near / (near - far), 0);
        var mvp = model * view * projection;
        gl.UseProgram(program);
        // Row-major System.Numerics matrices become the transposed column-major GLSL matrices.
        gl.UniformMatrix4fv(Location("model"), 1, 0, (float*)&model);
        gl.UniformMatrix4fv(Location("mvp"), 1, 0, (float*)&mvp);
        Uniform("eye", eye); Uniform("lightPosition", lightPosition);
        Uniform("lightColor", lightColor); Uniform("materialColor", color);
        gl.Uniform1f(Location("ambient"), ambient);
        gl.Uniform1f(Location("shininess"), shininess);
        gl.BindVertexArray(meshes[mesh].Vao);
        gl.DrawElements(0x0004, (int)meshes[mesh].Count, 0x1405, (void*)0);
    }
    private int Location(string name)
    {
        byte[] utf8 = Encoding.UTF8.GetBytes(name + '\0');
        fixed (byte* pointer = utf8) return Api.GetUniformLocation(program, pointer);
    }
    private void Uniform(string name, Vector3 value) => Api.Uniform3f(Location(name), value.X, value.Y, value.Z);
    private uint Shader(uint type, string source)
    {
        uint shader = Api.CreateShader(type);
        byte[] utf8 = Encoding.UTF8.GetBytes(source + '\0');
        fixed (byte* pointer = utf8)
        {
            byte* text = pointer;
            Api.ShaderSource(shader, 1, &text, null);
        }
        Api.CompileShader(shader);
        int status;
        Api.GetShaderiv(shader, 0x8B81, &status);
        if (status == 0)
        {
            var message = Log(shader, false);
            Api.DeleteShader(shader);
            throw new InvalidOperationException($"Shader compilation failed: {message}");
        }
        return shader;
    }
    private uint LinkProgram()
    {
        uint vertex = Shader(0x8B31, VertexShader), fragment = 0, linked = 0;
        try
        {
            fragment = Shader(0x8B30, FragmentShader);
            linked = Api.CreateProgram();
            Api.AttachShader(linked, vertex); Api.AttachShader(linked, fragment);
            Api.LinkProgram(linked);
            int status;
            Api.GetProgramiv(linked, 0x8B82, &status);
            if (status == 0) throw new InvalidOperationException($"Shader linking failed: {Log(linked, true)}");
            return linked;
        }
        catch { if (linked != 0) Api.DeleteProgram(linked); throw; }
        finally { Api.DeleteShader(vertex); if (fragment != 0) Api.DeleteShader(fragment); }
    }
    private string Log(uint value, bool isProgram)
    {
        byte[] bytes = new byte[4096];
        int length;
        fixed (byte* pointer = bytes)
        {
            if (isProgram) Api.GetProgramInfoLog(value, bytes.Length, &length, pointer);
            else Api.GetShaderInfoLog(value, bytes.Length, &length, pointer);
        }
        return Encoding.UTF8.GetString(bytes, 0, Math.Clamp(length, 0, bytes.Length));
    }
    public void Dispose()
    {
        if (disposed) return;
        try
        {
            if (api is not null)
            {
                Glfw.MakeContextCurrent(window);
                foreach (var mesh in meshes)
                {
                    uint vao = mesh.Vao, vbo = mesh.Vbo, ebo = mesh.Ebo;
                    api.DeleteVertexArrays(1, &vao); api.DeleteBuffers(1, &vbo); api.DeleteBuffers(1, &ebo);
                }
                if (program != 0) api.DeleteProgram(program);
            }
        }
        finally { disposed = true; Glfw.DestroyWindow(window); Glfw.Terminate(); }
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

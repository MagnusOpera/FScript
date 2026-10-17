using System.Numerics;

namespace FScript.Ffi.Rendering;

// This interface is behind the FFI boundary. Scripts only see the attributed F# records.
public interface IGraphics
{
    int Upload(float[] vertices, uint[] indices);
    void Clear(Vector3 color);
    void Camera(Vector3 eye, Vector3 target, float fieldOfView);
    void Light(Vector3 position, Vector3 color, float ambient);
    void Draw(int mesh, Vector3 rotation, Vector3 color, float shininess);
}

// Exercises the real script and adapters without a graphics context or native window.
public sealed class RecordingGraphics : IGraphics
{
    public List<(float[] Vertices, uint[] Indices)> Meshes { get; } = [];
    public List<Vector3> Rotations { get; } = [];
    public int Clears { get; private set; }
    public int Cameras { get; private set; }
    public int Lights { get; private set; }
    public int Upload(float[] vertices, uint[] indices)
    {
        Meshes.Add((vertices, indices));
        return Meshes.Count - 1;
    }
    public void Clear(Vector3 color) => Clears++;
    public void Camera(Vector3 eye, Vector3 target, float fieldOfView) => Cameras++;
    public void Light(Vector3 position, Vector3 color, float ambient) => Lights++;
    public void Draw(int mesh, Vector3 rotation, Vector3 color, float shininess)
    {
        if (mesh < 0 || mesh >= Meshes.Count) throw new ArgumentOutOfRangeException(nameof(mesh));
        Rotations.Add(rotation);
    }
}

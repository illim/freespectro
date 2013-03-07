varying vec4 pos;
attribute vec4 gl_Vertex;
void main()
{
  pos = gl_Vertex;
  gl_Position = ftransform();
} 
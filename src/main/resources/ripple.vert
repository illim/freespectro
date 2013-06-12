#version 130

varying float posx;
void main()
{
	gl_TexCoord[0] = gl_TextureMatrix[0] * gl_MultiTexCoord0;
	gl_Position = ftransform();
	posx = gl_Position.x;
}

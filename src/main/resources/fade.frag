#version 130

uniform sampler2D tex;
uniform float fact;
varying vec4 col;
vec4 base = vec4(0, 0, 0, 0);

void main() {
	vec4 color = texture2D(tex,gl_TexCoord[0].st);
  vec4 c = mix(base, min(color, col), fact);
  c[3] = color[3];
	gl_FragColor = c;
}

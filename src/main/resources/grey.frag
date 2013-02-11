uniform sampler2D tex;

varying vec4 col;

float low(float x) {
  return x - 0.5;
}

void main() {
	vec4 color = texture2D(tex,gl_TexCoord[0].st);
	vec4 base = vec4(low(col[0]), low(col[1]), low(col[2]), color[3]);
	gl_FragColor = mix(color, base, 0.5);
}


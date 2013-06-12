#version 130

uniform sampler2D tex;
uniform int cursor;
const vec2 c = vec2(0.5, 0.5);
varying float posx;

void main() {
  vec2 p = gl_TexCoord[0].xy;
  float cdist = distance(vec2((posx - 0.2)* 10, 0) + p, c)* (120 - cursor);
  float h = sin(cdist)/cdist;
  vec4 color = texture2D(tex, p + h * (c - p));
  gl_FragColor = color + h * vec4(1, 1, 1, 1);
}

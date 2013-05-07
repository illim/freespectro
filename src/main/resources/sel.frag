#version 130

uniform float size;
uniform float cursor;
uniform vec2 offset;
varying vec4 pos;

void main()
{
  vec2 p = ( (pos.xy - offset) / vec2(size) ) - vec2(0.5);
  vec2 pn = normalize(p);
  float alpha = length(cross(vec3(pn[0], pn[1], 0), vec3(1, 0, 0)));
  float color1 = 1/(1 + 5* length(p) + cos(alpha*20 + cursor/5));
  float color2 = 1/(1 + 10* length(p) + cos(alpha*10 + cursor/5));
  float color = (clamp(pow(color1 + color2, 3), 0.1, 1) - 0.1) * 1.1;
  gl_FragColor = vec4(color, color, color, color);
}
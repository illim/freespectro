
uniform sampler2D tex;
uniform vec2 grads[100];
uniform float width;
uniform float height;
uniform int cursor;

float wrapf(float x)        { return mod(x, 10);}
int  wrap(int x)            { return int(wrapf(x));}
int  coordToStep(float c)   { return wrap(int(c));}
vec2 tograd(int[2] gradi)   { return grads[gradi[0] +cursor + 10 * gradi[1]]; }
int[2] coordToGradi(vec2 c) { return int[2](coordToStep(c[0]), coordToStep(c[1])); }
float lerp(float t, float a, float b) { return a + t * (b - a) ; }
float ease(float x)         { return 3 * pow(x, 2) - 2 * pow( x, 3);}
vec2 gradMove(int[2] gradi, int x, int y){
  return tograd(int[2](wrap(gradi[0] + x), wrap(gradi[1] + y)));
}

float nz(vec2 coord){
  int[2] gradi = coordToGradi(coord);
  vec2 g1 = tograd(gradi);
  vec2 g2 = gradMove(gradi, 1, 0);
  vec2 g3 = gradMove(gradi, 1, 1);
  vec2 g4 = gradMove(gradi, 0, 1);

  vec2 i = vec2(wrapf(coord[0]) - gradi[0], wrapf(coord[1]) - gradi[1]);
  vec2 j = vec2(i[0] - 1, i[1]);
  vec2 k = vec2(i[0] - 1, i[1] - 1);
  vec2 l = vec2(i[0], i[1] - 1);
  
  float s = dot(g1, i);
  float t = dot(g2, j);
  float u = dot(g3, k);
  float v = dot(g4, l);
  
  float sx = ease(i[0]);
  float a = lerp(sx, s, t);
  float b = lerp(sx, v, u);
  float sy = ease(i[1]);
  return lerp(sy, a, b);
}

void main() {
  vec2 coord = gl_TexCoord[0].st;
  vec4 color = texture2D(tex,coord);
  float scale = 10;
  color[3] = nz(vec2(coord[0] * width / scale, coord[1] * height/ scale)) + color[3];
  gl_FragColor = color;
}

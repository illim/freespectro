uniform sampler2D tex;
const int gradwidth = 10;
uniform vec2 grads[gradwidth * gradwidth];
uniform float width;
uniform float height;
uniform int cursor;

float wrapf(float x)        { return mod(x, gradwidth);}
int  wrap(int x)            { return int(wrapf(x));}
int  coordToStep(float c)   { return wrap(int(c));}
vec2 tograd(int[2] gradi)   { return grads[gradi[0] + gradwidth * gradi[1]]; }
int[2] coordToGradi(vec2 c) { return int[2](coordToStep(c.x), coordToStep(c.y)); }
float lerp(float t, float a, float b) { return a + t * (b - a) ; }
float ease(float x)         { return 3 * pow(x, 2) - 2 * pow( x, 3);}
vec2 gradMove(int[2] gradi, int x, int y){
  return tograd(int[2](wrap(gradi[0] + x), wrap(gradi[1] + y)));
}

float getNoise(vec2 coord){
  int[2] gradi = coordToGradi(coord);
  vec2 g1 = tograd(gradi);
  vec2 g2 = gradMove(gradi, 1, 0);
  vec2 g3 = gradMove(gradi, 1, 1);
  vec2 g4 = gradMove(gradi, 0, 1);

  vec2 i = vec2(wrapf(coord.x) - gradi[0], wrapf(coord.y) - gradi[1]);
  vec2 j = vec2(i.x - 1, i.y);
  vec2 k = vec2(i.x - 1, i.y - 1);
  vec2 l = vec2(i.x, i.y - 1);

  float s = dot(g1, i);
  float t = dot(g2, j);
  float u = dot(g3, k);
  float v = dot(g4, l);

  float sx = ease(i.x);
  float a = lerp(sx, s, t);
  float b = lerp(sx, v, u);
  float sy = ease(i.y);
  return lerp(sy, a, b);
}

const float scale = 5;
const float nbframe= 50;
const float fadeperiod = 2 * 3.14/nbframe;
float fade(float f, int c){ return f * (1 - cos(c * fadeperiod)); }
float rescale(float x, float fact, int c){
  float k = fact / (scale + c);
  return gradwidth + gradwidth/2 + (x - 0.5) * k;// crap (center, scale, translate to map on grad)
}
float getFadeNoise(vec2 coord, int c){
  vec2 sized = vec2(rescale(coord.x, width, c), rescale(coord.y, height, c));
  return fade(getNoise( sized), c);
}

void main() {
  vec2 coord = gl_TexCoord[0].st;
  vec4 color = texture2D(tex,coord);

  if (color[3] != 0){
    float noiz  = getFadeNoise(coord, cursor);
    float noiz2 = getFadeNoise(coord, int(mod(cursor + nbframe / 2, nbframe)));
    color = color * (noiz + noiz2);
  }
  gl_FragColor = color;
}

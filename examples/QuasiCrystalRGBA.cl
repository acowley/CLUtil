float wave(float,float,float,float);
float wrap(float);

__kernel void quasiCrystal(int pixels, 
                           float scale,
                           float phase,
                           int numAngles,
                           __constant float *angles,
                           __global uchar *img) {
  int x = get_global_id(0);
  int y = get_global_id(1);
  float denom = (float)pixels - 1;
  float u = scale * ((2 * (float)x / denom) - 1);
  float v = scale * ((2 * (float)y / denom) - 1);
  float sum = 0.0f;
  for(int i = 0; i < numAngles; ++i) {
    sum += wave(phase, angles[i], u, v);
  }
  uchar r = (uchar)(255.0f * clamp(wrap(sum), 0.0f, 1.0f));
  vstore4((uchar4)(255,r,128,r), y*pixels + x, img);
}

float wrap(float n) {
  int k = (int)n;
  if(n < 0) k -= 1;
  float v = n - (float)k;
  if(k%2 == 1) v = 1 - v;
  return v;
}

float wave(float phase, float theta, float x, float y) {
  return (cos(cos(theta) * x + sin(theta)*y + phase) + 1) / 2;
}

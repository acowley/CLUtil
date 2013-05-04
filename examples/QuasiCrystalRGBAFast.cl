float wrap(float);

__kernel void quasiCrystal(int pixels, 
                           float scale,
                           float phase,
                           __global float *cosAngles,
                           __global float *sinAngles,
                           __global uchar *img) {
  int x = get_global_id(0);
  int y = get_global_id(1);
  float denom = (float)pixels - 1;
  float u = scale * ((2 * (float)x / denom) - 1);
  float v = scale * ((2 * (float)y / denom) - 1);

  /* If we know there are only 7 angles, then the host can pad
   * precomputed sine and cosine vectors with an extra 0 and we can
   * operate on float8 vectors. */

  float8 cosines = vload8(0, cosAngles);
  float8 sines = vload8(0, sinAngles);
 
  float8 waves = (cos(cosines*u + sines*v + phase) + 1.0f) / 2.0f;
  float sum = waves.s0 + waves.s1 + waves.s2 + waves.s3 + waves.s4 + waves.s5 + waves.s6;
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

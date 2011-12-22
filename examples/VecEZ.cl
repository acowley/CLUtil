__kernel void vecAdd(__global float *a, __global float *b, __global float *c) {
  size_t i = get_global_id(0);
  float4 r = vload4(i,a) + vload4(i,b);
  vstore4(r*r,i,c);
}

__kernel void funnyBusiness(__global double *a,
                            __global double *b,
                            __global double *c,
                            __global int *s) {
  size_t i = get_global_id(0);
  double4 r = vload4(i,a) + vload4(i,b);
  double4 rsq = r * r;
  vstore4(rsq, i, c);
  s[i] = (int)(rsq.x + rsq.y + rsq.z + rsq.w);
}

__kernel void floaty(__global float *a, __global float *b, __global float *c) {
  size_t x = get_global_id(0);
  size_t y = get_global_id(1);
  size_t i = y * get_global_size(1) + x;
  c[i] = a[i] + b[i];
}

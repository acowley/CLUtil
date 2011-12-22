__kernel void floaty(__global float *a, __global float *b, __global float *c) {
  size_t x = get_global_id(0);
  size_t y = get_global_id(1);
  size_t i = y * get_global_size(1) + x;
  c[i] = a[i] + b[i];
}

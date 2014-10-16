__kernel void vadd2D(global float *a,
                     global float *b,
                     global float *c) {
  size_t x = get_global_id(0);
  size_t y = get_global_id(1);
  size_t i = y * get_global_size(0) + x;
  c[i] = a[i] + b[i];
}

__kernel void vadd1D(global float *a,
                     global float *b,
                     global float *c) {
  size_t i = get_global_id(0);
  c[i] = a[i] + b[i];
}



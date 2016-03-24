
__kernel void kernel1(__global int *x, global int *y, int z)
{
    // }
    x[get_global_id(0)] = y[get_global_id(0)] * z;
}

__kernel __attribute__((reqd_work_group_size(8, 1, 1))) void kernel2(global int *x)
{
    x[get_global_id(0)] = 0;
}

__kernel void __attribute__((reqd_work_group_size(16, 1, 1))) kernel3(global int *x)
{
    x[get_global_id(0)] = 1;
}

__attribute__((reqd_work_group_size(32, 1, 1))) kernel void kernel4(global int *x)
{
    x[get_global_id(0)] = 1;
}

kernel void empty()
{
}

kernel void params(
    global float *out,
    global float2 *fs,
    local float *tile,
    constant short *coeffs,
    __constant short *coeffs,
    image2d_t img,
    sampler_t sampler,
    event_t evt,
    uint4 k)
{
    float sum = 0.0f;
    for (int i = 0; i < k; i++) {
        float2 f2 = fs[i + get_global_id(0)];
        sum += sqrt(f2.x*f2.x + f2.y*f2.y);
    }
    fs[get_global_id(0)] = sum;
}

kernel void mods(
    global uchar16 *buf,
    const uint8 coeffs,
    const volatile global char *b2,
    global const volatile char *b3)
{
    // } LBRACK is part of comment
    // so is "
    // so is */
    buf[get_global_id(0)] = ("0123456789\"}")[coeffs.x];
    // so is /*
}

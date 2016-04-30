
kernel void blend4(
    int x,
    global float *dst,
    global float *src1,
    global float *src2,
    float t)
{
    int id = get_global_id(1)*get_global_size(0) + get_global_id(0);
    dst[id] = mix(src1[id], src2[id], t);
}

/*
kernel void blend_x(
    global uchar4 *dst,
    global uchar4 *src1,
    global uchar4 *src2,
    float t)
{
    int id = get_global_id(1)*get_global_size(0) + get_global_id(0);
    dst[id].x = mix(src1[id].x, src2[id].x, t);
}

kernel void blend_y(
    global uchar4 *dst,
    global uchar4 *src1,
    global uchar4 *src2,
    float t)
{
    int id = get_global_id(1)*get_global_size(0) + get_global_id(0);
    dst[id].y = mix(src1[id].y, src2[id].y, t);
}

kernel void blend_z(
    global uchar4 *dst,
    global uchar4 *src1,
    global uchar4 *src2,
    float t)
{
    int id = get_global_id(1)*get_global_size(0) + get_global_id(0);
    dst[id].z = mix(src1[id].z, src2[id].z, t);
}

kernel void blend_w(
    global uchar4 *dst,
    global uchar4 *src1,
    global uchar4 *src2,
    float t)
{
    int id = get_global_id(1)*get_global_size(0) + get_global_id(0);
    dst[id].w = mix(src1[id].w, src2[id].w, t);
}
*/

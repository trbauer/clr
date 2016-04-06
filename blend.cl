
kernel void blend(
    global uchar4 *dst,
    global uchar4 *src1,
    global uchar4 *src2,
    float t)
{
    int id = get_global_id(1) * get_global_size(0) + get_global_id(0);
    dst[id] = mix(src1[id], src2[id], t);
}

__kernel void kernel(__global int *xs, int z)
{
    xs[get_global_id(0)] = z;
}

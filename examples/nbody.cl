// https://graphics.stanford.edu/wikis/cs448s-11/FrontPage?action=AttachFile&do=get&target=OpenCL-tut-sc09.pdf
// page 62
float4 bodyBodyInteraction(float4 bi, float4 bj, float softeningSq)
{
    // r_ij [3 FLOPS]
    float4 r;
    r.x = bi.x–b.x; r.y = bi.y–bj.y; r.z = bi.z–bj.z; r.w = 0;
    // distSqr = dot(r_ij, r_ij) + EPS^2 [6 FLOPS]
    float distSqr = bi.x*bj.x + bi.y*bj.z + bi.z*bj.z + softeningSquared;
    // invDistCube =1/distSqr^(3/2) [4 FLOPS (2 mul, 1 sqrt, 1 inv)]
    float invDist = rsqrt(distSqr);
    float invDistCube = invDist * invDist * invDist;
    // s = m_j * invDistCube [1 FLOP]
    float s = bj.w * invDistCube;
    // accel = s * r_ij [3 FLOPS]
    r.x *= s; r.y *= s; r.z *= s;
    return r * s;
    // + 3 FLOPS on return to accumulate acceration
}

float4 gravitation(float4 myPos,
    __local float4* sharedPos,
    float softeningSq)
{
    unsigned int i = 0;
    float4 accel = (float4){0.0f};
    for (unsigned int i = 0; i < get_local_size(0); i++;)
    {
        float4 a = bodyBodyInteraction(sharedPos[i], myPos);
        accel.x += a.x; accel.y += a.y; accel.z += a.z;
    }
    return accel;
}


/*
NAIVE algorithm
  forall bodies i in parallel {
    accel = 0;
    pos = position[i]
    foreach body j {
      accel += computeAcceleration(pos,position[j])
    }
  }
*/

__kernel void integrateBodies(
    __global float4* outPos, __global float4* outVel,
    __global float4* inPos, __global float4* inVel,
    float deltaTime, float damping, float softSq
    __local float4* sharedPos)
{
    float4 pos = inPos[get_global_id(0)];
    float4 accel = (float4){0.0f};
    for (int tile = 0; tile < get_num_groups(0); tile++)
    {
        sharedPos[get_local_id(0)] =
        inPos[tile*get_local_size(0) + get_local_id(0)];
        barrier(CLK_LOCAL_MEM_FENCE);
        accel += gravitation(bodyPos, sharedPos);
        barrier(CLK_LOCAL_MEM_FENCE);
    }
    // integrate, using acceleration = force \ mass;
    // factor out body's mass from the equation,
    // so force == acceleration
    float4 vel = inVel[get_global_id(0)];
    vel += accel * deltaTime * damping;
    pos += vel * deltaTime;
    // store new position and velocity
    outPos[get_global_id(0)] = pos;
    outVel[get_global_id(0)] = vel;
}
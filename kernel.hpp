#pragma once

#include "ap_int.h"

const int MEM_SIZE = 32 * 1024;

extern "C" {
void kernel(
  ap_uint<8> mem[MEM_SIZE],
  ap_uint<32>* gp
);
}

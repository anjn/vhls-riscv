#include <fstream>
#include <random>

#include "kernel.hpp"

const auto tests = {
  "rv32mi-p-csr.hex",
  "rv32mi-p-scall.hex",
  "rv32ui-p-add.hex",
  "rv32ui-p-addi.hex",
  "rv32ui-p-and.hex",
  "rv32ui-p-andi.hex",
  "rv32ui-p-auipc.hex",
  "rv32ui-p-beq.hex",
  "rv32ui-p-bge.hex",
  "rv32ui-p-bgeu.hex",
  "rv32ui-p-blt.hex",
  "rv32ui-p-bltu.hex",
  "rv32ui-p-bne.hex",
  "rv32ui-p-jal.hex",
  "rv32ui-p-jalr.hex",
  "rv32ui-p-lui.hex",
  "rv32ui-p-lw.hex",
  "rv32ui-p-or.hex",
  "rv32ui-p-ori.hex",
  "rv32ui-p-sll.hex",
  "rv32ui-p-slli.hex",
  "rv32ui-p-slt.hex",
  "rv32ui-p-slti.hex",
  "rv32ui-p-sltiu.hex",
  "rv32ui-p-sltu.hex",
  "rv32ui-p-sra.hex",
  "rv32ui-p-srai.hex",
  "rv32ui-p-srl.hex",
  "rv32ui-p-srli.hex",
  "rv32ui-p-sub.hex",
  "rv32ui-p-sw.hex",
  "rv32ui-p-xor.hex",
  "rv32ui-p-xori.hex",
};

int main(int argc, char** argv)
{
  ap_uint<8> mem[MEM_SIZE];

  for (auto t : tests) {
    {
      std::ifstream is(t);
      assert(is);
      for (int i = 0; i < MEM_SIZE; i++) {
        int d; is >> std::hex >> d; mem[i] = d;
        if (is.eof()) break;
      }
    }

    ap_uint<32> gp;
    kernel(mem, &gp);

    std::cout << t << ",\tgp = " << gp << std::endl;

    bool pass = true;
    if (gp != 1) pass = false;
    if (!pass) return EXIT_FAILURE;
  }
}

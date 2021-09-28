#ifndef __SYNTHESIS__
#include <iostream>
#endif

#include "kernel.hpp"

template<int N>
struct BitPatImpl {
  const char (&pat)[N];
  inline bool operator==(const ap_uint<N-2>& v) const {
#pragma HLS INLINE
#pragma HLS ARRAY_PARTITION variable=pat dim=1 type=complete
    bool result = true;
    for (int i = 0; i < N - 2; i++) {
#pragma HLS UNROLL
      result = result && (pat[i+1] == '?' || v[N-3-i] == (pat[i+1] == '1' ? 1 : 0));
    }
    return result;
  }
};

template<int N>
constexpr BitPatImpl<N> BitPat(const char (&pat)[N]) {
  return BitPatImpl<N>{ pat };
}

// ロード・ストア
constexpr auto LW      = BitPat("b?????????????????010?????0000011");
constexpr auto SW      = BitPat("b?????????????????010?????0100011");

// 加算
constexpr auto ADD     = BitPat("b0000000??????????000?????0110011");
constexpr auto ADDI    = BitPat("b?????????????????000?????0010011");

// 減算
constexpr auto SUB     = BitPat("b0100000??????????000?????0110011");

// 論理演算
constexpr auto AND     = BitPat("b0000000??????????111?????0110011");
constexpr auto OR      = BitPat("b0000000??????????110?????0110011");
constexpr auto XOR     = BitPat("b0000000??????????100?????0110011");
constexpr auto ANDI    = BitPat("b?????????????????111?????0010011");
constexpr auto ORI     = BitPat("b?????????????????110?????0010011");
constexpr auto XORI    = BitPat("b?????????????????100?????0010011");

// シフト
constexpr auto SLL     = BitPat("b0000000??????????001?????0110011");
constexpr auto SRL     = BitPat("b0000000??????????101?????0110011");
constexpr auto SRA     = BitPat("b0100000??????????101?????0110011");
constexpr auto SLLI    = BitPat("b0000000??????????001?????0010011");
constexpr auto SRLI    = BitPat("b0000000??????????101?????0010011");
constexpr auto SRAI    = BitPat("b0100000??????????101?????0010011");

// 比較
constexpr auto SLT     = BitPat("b0000000??????????010?????0110011");
constexpr auto SLTU    = BitPat("b0000000??????????011?????0110011");
constexpr auto SLTI    = BitPat("b?????????????????010?????0010011");
constexpr auto SLTIU   = BitPat("b?????????????????011?????0010011");

// 条件分岐
constexpr auto BEQ     = BitPat("b?????????????????000?????1100011");
constexpr auto BNE     = BitPat("b?????????????????001?????1100011");
constexpr auto BLT     = BitPat("b?????????????????100?????1100011");
constexpr auto BGE     = BitPat("b?????????????????101?????1100011");
constexpr auto BLTU    = BitPat("b?????????????????110?????1100011");
constexpr auto BGEU    = BitPat("b?????????????????111?????1100011");

// ジャンプ
constexpr auto JAL     = BitPat("b?????????????????????????1101111");
constexpr auto JALR    = BitPat("b?????????????????000?????1100111");

// 即値ロード
constexpr auto LUI     = BitPat("b?????????????????????????0110111");
constexpr auto AUIPC   = BitPat("b?????????????????????????0010111");

// CSR
constexpr auto CSRRW   = BitPat("b?????????????????001?????1110011");
constexpr auto CSRRWI  = BitPat("b?????????????????101?????1110011");
constexpr auto CSRRS   = BitPat("b?????????????????010?????1110011");
constexpr auto CSRRSI  = BitPat("b?????????????????110?????1110011");
constexpr auto CSRRC   = BitPat("b?????????????????011?????1110011");
constexpr auto CSRRCI  = BitPat("b?????????????????111?????1110011");

// 例外
constexpr auto ECALL   = BitPat("b00000000000000000000000001110011");

constexpr int ALU_X      =  0;
constexpr int ALU_ADD    =  1;
constexpr int ALU_SUB    =  2;
constexpr int ALU_AND    =  3;
constexpr int ALU_OR     =  4;
constexpr int ALU_XOR    =  5;
constexpr int ALU_SLL    =  6;
constexpr int ALU_SRL    =  7;
constexpr int ALU_SRA    =  8;
constexpr int ALU_SLT    =  9;
constexpr int ALU_SLTU   = 10;
constexpr int BR_BEQ     = 11;
constexpr int BR_BNE     = 12;
constexpr int BR_BLT     = 13;
constexpr int BR_BGE     = 14;
constexpr int BR_BLTU    = 15;
constexpr int BR_BGEU    = 16;
constexpr int ALU_JALR   = 17;
constexpr int ALU_COPY1  = 18;

constexpr int OP1_RS1    = 0;
constexpr int OP1_PC     = 1;
constexpr int OP1_X      = 2;
constexpr int OP1_IMZ    = 3;

constexpr int OP2_X      = 0;
constexpr int OP2_RS2    = 1;
constexpr int OP2_IMI    = 2;
constexpr int OP2_IMS    = 3;
constexpr int OP2_IMJ    = 4;
constexpr int OP2_IMU    = 5;

constexpr int MEN_X      = 0;
constexpr int MEN_S      = 1;

constexpr int REN_X      = 0;
constexpr int REN_S      = 1;

constexpr int WB_X       = 0;
constexpr int WB_ALU     = 0;
constexpr int WB_MEM     = 1;
constexpr int WB_PC      = 2;
constexpr int WB_CSR     = 3;

constexpr int CSR_X      = 0;
constexpr int CSR_W      = 1;
constexpr int CSR_S      = 2;
constexpr int CSR_C      = 3;
constexpr int CSR_E      = 4;

void kernel(
  ap_uint<8> mem[MEM_SIZE],
  ap_uint<32>* gp
) {

  ap_uint<8> local_mem[MEM_SIZE];
#pragma HLS ARRAY_RESHAPE variable=local_mem type=cyclic factor=4 dim=1
  ap_uint<32> regfile[32];
  ap_uint<32> csr_regfile[4096];
  ap_uint<32> pc_reg = 0;

memcpy:
  for (int i = 0; i < MEM_SIZE; i++) {
    local_mem[i] = mem[i];
  }

clear_regfile:
  for (int i = 0; i < 32; i++) {
    regfile[i] = 0;
  }

clear_csr_regfile:
  for (int i = 0; i < 4096; i++) {
    csr_regfile[i] = 0;
  }

core:
  while (true) {
    // Test exit condition
    if (pc_reg == 0x44) break;

    ap_uint<32> inst;
    inst( 7,  0) = (ap_uint<8>) local_mem[pc_reg + 0];
    inst(15,  8) = (ap_uint<8>) local_mem[pc_reg + 1];
    inst(23, 16) = (ap_uint<8>) local_mem[pc_reg + 2];
    inst(31, 24) = (ap_uint<8>) local_mem[pc_reg + 3];

    ap_uint<5> rs1_addr = inst(19, 15);
    ap_uint<5> rs2_addr = inst(24, 20);
    ap_uint<5> wb_addr = inst(11, 7);

    ap_uint<32> rs1_data = rs1_addr == 0 ? ap_uint<32>(0) : regfile[rs1_addr];
    ap_uint<32> rs2_data = rs2_addr == 0 ? ap_uint<32>(0) : regfile[rs2_addr];

    ap_int<12>  imm_i = inst(31, 20);
    ap_int<12>  imm_s = (inst(31, 25), inst(11, 7));
    ap_int<12>  imm_b = (inst[31], inst[7], inst(30, 25), inst(11, 8));
    ap_int<12>  imm_j = (inst[31], inst(19,12), inst[20], inst(30, 21));
    ap_uint<20> imm_u = inst(31, 12);
    ap_uint<5>  imm_z = inst(19, 15);

    ap_int<13> imm_b_sext = (imm_b << 1);
    ap_int<13> imm_j_sext = (imm_j << 1);
    ap_uint<32> imm_u_shifted = ap_uint<32>(imm_u) << 12;

    int exe_fun = ALU_X, op1_sel = OP1_RS1, op2_sel = OP2_RS2, mem_wen = MEN_X;
    int rf_wen = REN_X, wb_sel = WB_X, csr_cmd = CSR_X;
#define DECODE_INST(x, alu, op1, op2, men, ren, wb, csr) \
    if (x == inst) { exe_fun = alu; op1_sel = op1; \
      op2_sel = op2; mem_wen = men; rf_wen = ren; wb_sel = wb; csr_cmd = csr; }
    DECODE_INST(LW    , ALU_ADD  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_MEM, CSR_X)
    DECODE_INST(SW    , ALU_ADD  , OP1_RS1, OP2_IMS, MEN_S, REN_X, WB_X  , CSR_X)
    DECODE_INST(ADD   , ALU_ADD  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X)
    DECODE_INST(ADDI  , ALU_ADD  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X)
    DECODE_INST(SUB   , ALU_SUB  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X)
    DECODE_INST(AND   , ALU_AND  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X)
    DECODE_INST(OR    , ALU_OR   , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X)
    DECODE_INST(XOR   , ALU_XOR  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X)
    DECODE_INST(ANDI  , ALU_AND  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X)
    DECODE_INST(ORI   , ALU_OR   , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X)
    DECODE_INST(XORI  , ALU_XOR  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X)
    DECODE_INST(SLL   , ALU_SLL  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X)
    DECODE_INST(SRL   , ALU_SRL  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X)
    DECODE_INST(SRA   , ALU_SRA  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X)
    DECODE_INST(SLLI  , ALU_SLL  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X)
    DECODE_INST(SRLI  , ALU_SRL  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X)
    DECODE_INST(SRAI  , ALU_SRA  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X)
    DECODE_INST(SLT   , ALU_SLT  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X)
    DECODE_INST(SLTU  , ALU_SLTU , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X)
    DECODE_INST(SLTI  , ALU_SLT  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X)
    DECODE_INST(SLTIU , ALU_SLTU , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X)
    DECODE_INST(BEQ   , BR_BEQ   , OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X  , CSR_X)
    DECODE_INST(BNE   , BR_BNE   , OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X  , CSR_X)
    DECODE_INST(BGE   , BR_BGE   , OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X  , CSR_X)
    DECODE_INST(BGEU  , BR_BGEU  , OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X  , CSR_X)
    DECODE_INST(BLT   , BR_BLT   , OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X  , CSR_X)
    DECODE_INST(BLTU  , BR_BLTU  , OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X  , CSR_X)
    DECODE_INST(JAL   , ALU_ADD  , OP1_PC , OP2_IMJ, MEN_X, REN_S, WB_PC , CSR_X)
    DECODE_INST(JALR  , ALU_JALR , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_PC , CSR_X)
    DECODE_INST(LUI   , ALU_ADD  , OP1_X  , OP2_IMU, MEN_X, REN_S, WB_ALU, CSR_X)
    DECODE_INST(AUIPC , ALU_ADD  , OP1_PC , OP2_IMU, MEN_X, REN_S, WB_ALU, CSR_X)
    DECODE_INST(CSRRW , ALU_COPY1, OP1_RS1, OP2_X  , MEN_X, REN_S, WB_CSR, CSR_W)
    DECODE_INST(CSRRWI, ALU_COPY1, OP1_IMZ, OP2_X  , MEN_X, REN_S, WB_CSR, CSR_W)
    DECODE_INST(CSRRS , ALU_COPY1, OP1_RS1, OP2_X  , MEN_X, REN_S, WB_CSR, CSR_S)
    DECODE_INST(CSRRSI, ALU_COPY1, OP1_IMZ, OP2_X  , MEN_X, REN_S, WB_CSR, CSR_S)
    DECODE_INST(CSRRC , ALU_COPY1, OP1_RS1, OP2_X  , MEN_X, REN_S, WB_CSR, CSR_C)
    DECODE_INST(CSRRCI, ALU_COPY1, OP1_IMZ, OP2_X  , MEN_X, REN_S, WB_CSR, CSR_C)
    DECODE_INST(ECALL , ALU_X    , OP1_X  , OP2_X  , MEN_X, REN_X, WB_X  , CSR_E)
#undef DECODE_INST

    ap_uint<32> op1_data = 0;
    if (op1_sel == OP1_RS1) op1_data = rs1_data;
    if (op1_sel == OP1_PC ) op1_data = pc_reg  ;
    if (op1_sel == OP1_IMZ) op1_data = imm_z   ;

    ap_uint<32> op2_data = 0;
    if (op2_sel == OP2_RS2) op2_data = rs2_data     ;
    if (op2_sel == OP2_IMI) op2_data = imm_i        ;
    if (op2_sel == OP2_IMS) op2_data = imm_s        ;
    if (op2_sel == OP2_IMJ) op2_data = imm_j_sext   ;
    if (op2_sel == OP2_IMU) op2_data = imm_u_shifted;

    // Execute stage
    ap_uint<32> alu_out = 0;
    if (exe_fun == ALU_ADD  ) alu_out = op1_data + op2_data;
    if (exe_fun == ALU_SUB  ) alu_out = op1_data - op2_data;
    if (exe_fun == ALU_AND  ) alu_out = op1_data & op2_data;
    if (exe_fun == ALU_OR   ) alu_out = op1_data | op2_data;
    if (exe_fun == ALU_XOR  ) alu_out = op1_data ^ op2_data;
    if (exe_fun == ALU_SLL  ) alu_out = op1_data << op2_data(4, 0);
    if (exe_fun == ALU_SRL  ) alu_out = op1_data >> op2_data(4, 0);
    if (exe_fun == ALU_SRA  ) alu_out = ap_int<32>(op1_data) >> op2_data(4, 0);
    if (exe_fun == ALU_SLT  ) alu_out = ap_int<32>(op1_data) < ap_int<32>(op2_data);
    if (exe_fun == ALU_SLTU ) alu_out = op1_data < op2_data;
    if (exe_fun == ALU_JALR ) alu_out = (op1_data + op2_data) & ~1;
    if (exe_fun == ALU_COPY1) alu_out = op1_data;

    // Branch
    bool br_flg = false;
    ap_uint<32> br_target = pc_reg + imm_b_sext;
    if (exe_fun == BR_BEQ ) br_flg =  (op1_data == op2_data);
    if (exe_fun == BR_BNE ) br_flg = !(op1_data == op2_data);
    if (exe_fun == BR_BLT ) br_flg =  (ap_int<32>(op1_data) < ap_int<32>(op2_data));
    if (exe_fun == BR_BGE ) br_flg = !(ap_int<32>(op1_data) < ap_int<32>(op2_data));
    if (exe_fun == BR_BLTU) br_flg =  (op1_data < op2_data);
    if (exe_fun == BR_BGEU) br_flg = !(op1_data < op2_data);

    // CSR
    ap_uint<12> csr_addr = csr_cmd == CSR_E ? ap_uint<12>(0x342) : inst(31, 20);
    ap_uint<32> csr_rdata = csr_regfile[csr_addr];
    ap_uint<32> csr_wdata = 0;
    if (csr_cmd == CSR_W) csr_wdata = op1_data;
    if (csr_cmd == CSR_S) csr_wdata = csr_rdata |  op1_data;
    if (csr_cmd == CSR_C) csr_wdata = csr_rdata & ~op1_data;
    if (csr_cmd == CSR_E) csr_wdata = 11;

    if (csr_cmd > 0) csr_regfile[csr_addr] = csr_wdata;

    // Writeback stage
    ap_uint<32> wb_data = alu_out;

    if (mem_wen == MEN_S) {
      local_mem[alu_out + 0] = rs2_data( 7,  0);
      local_mem[alu_out + 1] = rs2_data(15,  8);
      local_mem[alu_out + 2] = rs2_data(23, 16);
      local_mem[alu_out + 3] = rs2_data(31, 24);
    } else if (wb_sel == WB_MEM) {
      wb_data( 7,  0) = (ap_uint<8>) local_mem[alu_out + 0];
      wb_data(15,  8) = (ap_uint<8>) local_mem[alu_out + 1];
      wb_data(23, 16) = (ap_uint<8>) local_mem[alu_out + 2];
      wb_data(31, 24) = (ap_uint<8>) local_mem[alu_out + 3];
    } else if (wb_sel == WB_PC) {
      wb_data = pc_reg + 4;
    } else if (wb_sel == WB_CSR) {
      wb_data = csr_rdata;
    }

    if (rf_wen == REN_S) {
      regfile[wb_addr] = wb_data;
    }

#ifndef __SYNTHESIS__
    std::cout << "===========" << std::endl;
    std::cout << "pc_reg   = " << std::setw(16) << std::hex << pc_reg << std::endl;
    std::cout << "inst     = " << std::setw(16) << std::hex << inst;
#define PRINT_INST(x) if (x == inst) std::cout << "  " << #x << std::endl; else
    PRINT_INST(LW    )
    PRINT_INST(SW    )
    PRINT_INST(ADD   )
    PRINT_INST(ADDI  )
    PRINT_INST(SUB   )
    PRINT_INST(AND   )
    PRINT_INST(OR    )
    PRINT_INST(XOR   )
    PRINT_INST(ANDI  )
    PRINT_INST(ORI   )
    PRINT_INST(XORI  )
    PRINT_INST(SLL   )
    PRINT_INST(SRL   )
    PRINT_INST(SRA   )
    PRINT_INST(SLLI  )
    PRINT_INST(SRLI  )
    PRINT_INST(SRAI  )
    PRINT_INST(SLT   )
    PRINT_INST(SLTU  )
    PRINT_INST(SLTI  )
    PRINT_INST(SLTIU )
    PRINT_INST(BEQ   )
    PRINT_INST(BNE   )
    PRINT_INST(BGE   )
    PRINT_INST(BGEU  )
    PRINT_INST(BLT   )
    PRINT_INST(BLTU  )
    PRINT_INST(JAL   )
    PRINT_INST(JALR  )
    PRINT_INST(LUI   )
    PRINT_INST(AUIPC )
    PRINT_INST(CSRRW )
    PRINT_INST(CSRRWI)
    PRINT_INST(CSRRS )
    PRINT_INST(CSRRSI)
    PRINT_INST(CSRRC )
    PRINT_INST(CSRRCI)
    PRINT_INST(ECALL )
    std::cout << "  unimpl" << std::endl;
#undef PRINT_INST

    std::cout << "rs1_addr = " << std::dec << rs1_addr << std::endl;
    std::cout << "rs2_addr = " << std::dec << rs2_addr << std::endl;
    std::cout << "rs1_data = " << std::hex << rs1_data << std::endl;
    std::cout << "rs2_data = " << std::hex << rs2_data << std::endl;
    std::cout << "alu_out  = " << std::hex << alu_out << std::endl;
    std::cout << "wb_addr  = " << std::dec << wb_addr << std::endl;
    std::cout << "wb_data  = " << std::hex << wb_data << std::endl;
#endif

    if (br_flg) {
      pc_reg = br_target;
    } else if (JAL == inst || JALR == inst) {
      pc_reg = alu_out;
    } else if (ECALL == inst) {
      pc_reg = csr_regfile[0x305];
    } else {
      pc_reg = pc_reg + 4;
    }
  }

  *gp = regfile[3];
}

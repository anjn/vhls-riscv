#!/usr/bin/env bash

UI_INSTS="sw lw add addi sub and andi or ori xor xori sll srl sra slli srli srai slt sltu slti sltiu beq bne blt bge bltu bgeu jal jalr lui auipc"
MI_INSTS="csr scall"

ISA_DIR=/riscv-tests/target/share/riscv-tests/isa

function gen_hex() {
  local prefix=$1
  local name=$2
  if [ ! -e $ISA_DIR/$prefix-$name ] ; then
    echo Error: $ISA_DIR/$prefix-$name not found!
    exit
  fi
  riscv64-unknown-elf-objcopy -O binary $ISA_DIR/$prefix-$name bin/$prefix-$name.bin
  od -An -tx1 -w1 -v bin/$prefix-$name.bin > hex/$prefix-$name.hex
}

mkdir -p bin
mkdir -p hex

for t in $UI_INSTS ; do
  gen_hex rv32ui-p $t
done

for t in $MI_INSTS ; do
  gen_hex rv32mi-p $t
done

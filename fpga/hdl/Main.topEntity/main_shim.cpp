#include <cstdlib>

#include <verilated.h>

#include "Vmain.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  Vmain *top = new Vmain;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}


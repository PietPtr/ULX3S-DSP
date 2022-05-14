#include <cstdlib>

#include <verilated.h>

#include "Vledmov.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  Vledmov *top = new Vledmov;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}


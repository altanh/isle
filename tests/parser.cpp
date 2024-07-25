#include <isle/parser.h>

#include <fstream>
#include <iostream>

int main(int argc, char **argv) {
  using namespace isle;

  std::vector<SExpr> sexprs;

  SExpr sexpr;
  while (std::cin >> sexpr) {
    sexprs.push_back(sexpr);
  }

  std::cout << "input program\n";
  std::cout << "=============\n";
  for (auto &sexpr : sexprs) {
    std::cout << sexpr << std::endl;
  }
  std::cout << "=============\n";
  std::cout << std::flush;

  std::cout << "parsing..." << std::endl << std::endl;

  auto prog = ParseProgram(sexprs);
  if (!prog) {
    std::cerr << "failed to parse program!" << std::endl;
    return 1;
  }
  prog.value().Print();

  return 0;
}
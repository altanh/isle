#include <isle/gen.h>
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
  if (std::cin.bad()) {
    std::cerr << "failed to parse s-expression!" << std::endl;
    return 1;
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

  std::cout << "generated code:\n";
  for (const auto &td : prog.value().type_decls) {
    EmitTypeDecl(prog.value(), td, std::cout);
  }
  std::cout << std::flush;

  return 0;
}
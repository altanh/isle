#include <isle/gen.h>
#include <isle/parser.h>

#include <fstream>
#include <iostream>

int main(int argc, char **argv) {
  using namespace isle;

  std::vector<SExpr> sexprs;

  std::ifstream is(argv[1]);
  std::ofstream os(argv[2]);

  SExpr sexpr;
  while (is >> sexpr) {
    sexprs.push_back(sexpr);
  }
  if (is.bad()) {
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

  for (const auto &td : prog.value().type_decls) {
    EmitTypeDecl(prog.value(), td, os);
  }

  for (const auto &fn : prog.value().fn_decls) {
    EmitConstructor(prog.value(), fn.id, os);
  }

  os << std::flush;

  return 0;
}
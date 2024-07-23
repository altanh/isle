#include <iostream>

#include <isle/ir.h>
#include <isle/visitors.h>

int main(int argc, char **argv) {
  using namespace isle;

  Expr no_fv = ECall(0, {ECall(1, {IntConst(0)}), IntConst(1)});
  Expr fv = ECall(0, {Var("x"), IntConst(1)});

  std::cout << std::boolalpha;
  std::cout << std::visit(HasFreeVars{}, no_fv) << "\n";
  std::cout << std::visit(HasFreeVars{}, fv) << "\n";
  std::cout << std::flush;

  std::cout << Printer::Print(no_fv) << std::endl;
  std::cout << Printer::Print(fv) << std::endl;

  return 0;
}
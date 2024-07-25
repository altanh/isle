#include <iostream>

#include <isle/ir.h>
#include <isle/visitors.h>

int main(int argc, char **argv) {
  using namespace isle;

  Expr no_fv = ECall(0, {ECall(1, {IntConst(0)}), IntConst(1)});
  Expr fv = ECall(0, {Var("x"), IntConst(1)});
  Expr fv2 = ECall(0, {Var("x"), IntConst(1)});
  Expr fv3 = ECall(0, {Var("y"), IntConst(1)});

  std::cout << std::boolalpha;
  std::cout << HasFreeVars(no_fv, {}) << "\n";
  std::cout << HasFreeVars(fv, {}) << "\n";
  std::cout << std::flush;

  std::cout << Printer::Print(no_fv) << std::endl;
  std::cout << Printer::Print(fv) << std::endl;

  std::cout << "fv == fv2: " << ExprEq::Check(fv, fv2) << std::endl;
  std::cout << "fv == no_fv: " << ExprEq::Check(fv, no_fv) << std::endl;
  std::cout << "fv == fv3: " << ExprEq::Check(fv, fv3) << std::endl;

  return 0;
}
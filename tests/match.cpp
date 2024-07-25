#include <iostream>
#include <vector>

#include <isle/ir.h>
#include <isle/match.h>

int main(int argc, char **argv) {
  using namespace isle;

  Var x("x");
  IntConst zero(0);
  IntConst one(1);

  PatternRef px = x.Clone();
  PatternRef pzero = zero.Clone();
  PatternRef pone = one.Clone();
  ExprRef ex = x.Clone();
  ExprRef ezero = zero.Clone();
  ExprRef eone = one.Clone();

  Pattern pat = PCall(0, {px, pone});
  Pattern pat2 = PCall(0, {pone, pone});
  Pattern pat3 = PCall(0, {px, px});

  Expr expr = ECall(0, {ezero, eone});
  Expr expr2 = ECall(0, {eone, eone});
  Expr expr3 = ECall(0, {ezero, ezero});

  using Test = std::tuple<Pattern, Expr, bool>;
  std::vector tests = {
      Test{pat, expr, true},   Test{pat, expr2, true},
      Test{pat, expr3, false}, Test{pat2, expr, false},
      Test{pat2, expr2, true}, Test{pat2, expr3, false},
      Test{pat3, expr, false}, Test{pat3, expr2, true},
      Test{pat3, expr3, true},
  };

  // TODO(@altanh): more tests (check fn equality, arg sizes, etc.)

  std::cout << std::boolalpha;

  int ret = 0;

  for (const Test &test : tests) {
    std::cout << "Match(" << Printer::Print(std::get<0>(test)) << ", "
              << Printer::Print(std::get<1>(test)) << ")\n";
    std::cout << "\tExpected: " << std::get<2>(test) << "\n";
    bool actual = Matcher::Match(std::get<0>(test), std::get<1>(test));
    bool pass = actual == std::get<2>(test);
    std::cout << "\tActual: " << actual << "\n";
    std::cout << "\tRESULT: " << (pass ? "PASS" : "FAIL !!!!!!!!") << "\n";
    if (!pass) {
      ret = 1;
    }
  }

  std::cout << std::flush;

  return ret;
}

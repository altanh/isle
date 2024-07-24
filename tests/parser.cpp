#include <isle/parser.h>

#include <iostream>

int main(int argc, char **argv) {
  using namespace isle;

  SExpr sexpr;
  while (std::cin >> sexpr) {
    std::cout << sexpr << std::endl;
  }

  return 0;
}
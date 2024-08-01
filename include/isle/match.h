#pragma once

#include <unordered_map>
#include <variant>

#include "ir.h"
#include "visitors.h"

namespace isle {

class Matcher {
  std::unordered_map<std::string, Expr> &bindings_;

public:
  bool operator()(const PCall &pcall, const ECall &ecall) {
    if (pcall.fn != ecall.fn) {
      return false;
    }
    if (pcall.args.size() != ecall.args.size()) {
      return false;
    }
    for (int i = 0; i < pcall.args.size(); ++i) {
      if (!std::visit(*this, *pcall.args[i], *ecall.args[i])) {
        return false;
      }
    }
    return true;
  }

  bool operator()(const PAnd &pand, const Expr &expr) {
    for (const auto &pat : pand.patterns) {
      if (!std::visit(*this, *pat, expr)) {
        return false;
      }
    }
    return true;
  }

  bool operator()(const PBind &pbind, const Expr &expr) {
    if (!std::visit(*this, *pbind.pattern, expr)) {
      return false;
    }
    bindings_.emplace(pbind.var.name, expr);
    return true;
  }

  bool operator()(const PWildcard &wc, const Expr &expr) { return true; }

  bool operator()(const Var &var, const Expr &expr) {
    if (bindings_.count(var.name)) {
      return Equals(bindings_.at(var.name), expr);
    }
    bindings_.emplace(var.name, expr);
    return true;
  }

  bool operator()(const IntConst &i, const IntConst &j) {
    return i.value == j.value;
  }

  template <typename P, typename E> bool operator()(const P &p, const E &e) {
    return false;
  }

  Matcher(std::unordered_map<std::string, Expr> &bindings)
      : bindings_(bindings) {}
};

bool Match(const Pattern &pattern, const Expr &expr) {
  std::unordered_map<std::string, Expr> bindings;
  return std::visit(Matcher(bindings), pattern, expr);
}

} // namespace isle
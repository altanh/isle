#pragma once

#include <unordered_map>
#include <variant>

#include "ir.h"
#include "visitors.h"

namespace isle {

struct Matcher {
  bool operator()(const PCall &p_call) {
    // check that expr is a ECall
    if (!std::holds_alternative<ECall>(expr)) {
      return false;
    }
    const ECall &e_call = std::get<ECall>(expr);
    // check fn equality
    if (p_call.fn != e_call.fn) {
      return false;
    }
    // TODO(@altanh): should we support variable number of args?
    if (p_call.args.size() != e_call.args.size()) {
      return false;
    }
    // match args
    for (size_t i = 0; i < p_call.args.size(); ++i) {
      if (!std::visit(Matcher{e_call.args[i], bindings}, p_call.args[i])) {
        return false;
      }
    }
    return true;
  }

  bool operator()(const PWildcard &p_wildcard) { return true; }

  bool operator()(const Var &var) {
    // if var is bound, check that it is bound to the same expr
    if (bindings->count(var.name)) {
      return ExprEq::Check(bindings->at(var.name), expr);
    }

    // bind var to expr
    bindings->emplace(var.name, expr);
    return true;
  }

  bool operator()(const IntConst &int_const) {
    return std::holds_alternative<IntConst>(expr) &&
           std::get<IntConst>(expr).value == int_const.value;
  }

  Matcher(const Expr &expr, std::unordered_map<std::string, Expr> *bindings)
      : expr(expr), bindings(bindings) {}

  static bool Match(const Pattern &pattern, const Expr &expr) {
    std::unordered_map<std::string, Expr> bindings;
    return std::visit(Matcher{expr, &bindings}, pattern);
  }

private:
  const Expr &expr;
  std::unordered_map<std::string, Expr> *bindings;
};

} // namespace isle
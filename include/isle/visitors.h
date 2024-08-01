#pragma once

#include <sstream>
#include <unordered_map>

#include "ir.h"

namespace isle {

struct ExprEq {
  bool operator()(const ECall &lhs, const ECall &rhs) {
    if (lhs.fn != rhs.fn) {
      return false;
    }
    if (lhs.args.size() != rhs.args.size()) {
      return false;
    }
    for (size_t i = 0; i < lhs.args.size(); ++i) {
      if (!std::visit(*this, *lhs.args[i], *rhs.args[i])) {
        return false;
      }
    }
    return true;
  }

  // TODO: consider alpha equivalence?
  bool operator()(const ELet &lhs, const ELet &rhs) {
    if (lhs.var.name != rhs.var.name) {
      return false;
    }
    if (!std::visit(*this, *lhs.value, *rhs.value)) {
      return false;
    }
    return std::visit(*this, *lhs.body, *rhs.body);
  }

  bool operator()(const Var &lhs, const Var &rhs) {
    return lhs.name == rhs.name;
  }

  bool operator()(const IntConst &lhs, const IntConst &rhs) {
    return lhs.value == rhs.value;
  }

  // mismatched variants
  template <typename L, typename R>
  bool operator()(const L &lhs, const R &rhs) {
    return false;
  }
};

bool Equals(const Expr &lhs, const Expr &rhs) {
  return std::visit(ExprEq{}, lhs, rhs);
}

// Example: check if Expr has free variables

bool HasFreeVars(const Expr &expr,
                 const std::unordered_map<std::string, int> &bindings) {
  return std::visit(
      Visitor{[&](const ECall &call) {
                for (auto arg : call.args) {
                  if (HasFreeVars(*arg, bindings)) {
                    return true;
                  }
                }
                return false;
              },
              [&](const ELet &let) {
                if (HasFreeVars(*let.value, bindings)) {
                  return true;
                }
                auto new_bindings(bindings);
                new_bindings[let.var.name]++;
                return HasFreeVars(*let.body, new_bindings);
              },
              [&](const Var &var) { return !bindings.count(var.name); },
              [&](const IntConst &i) { return false; }},
      expr);
}

void CollectVars(const Pattern &pat,
                 std::unordered_map<std::string, int> *result) {
  std::visit(Visitor{[&](const PCall &call) {
                       for (auto arg : call.args) {
                         CollectVars(*arg, result);
                       }
                     },
                     [&](const PAnd &pand) {
                       for (auto arg : pand.patterns) {
                         CollectVars(*arg, result);
                       }
                     },
                     [&](const PBind &bind) {
                       result->operator[](bind.var.name)++;
                       CollectVars(*bind.pattern, result);
                     },
                     [&](const PWildcard &w) {},
                     [&](const Var &var) { result->operator[](var.name)++; },
                     [&](const IntConst &i) {}},
             pat);
}

} // namespace isle
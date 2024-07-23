#pragma once

#include <sstream>
#include <unordered_map>

#include "ir.h"

namespace isle {

struct Printer {
  void operator()(const ECall &e_call) {
    oss << "($" << e_call.fn;
    for (const Expr &arg : e_call.args) {
      oss << " ";
      std::visit(*this, arg);
    }
    oss << ")";
  }

  void operator()(const PCall &p_call) {
    oss << "($" << p_call.fn;
    for (const Pattern &arg : p_call.args) {
      oss << " ";
      std::visit(*this, arg);
    }
    oss << ")";
  }

  void operator()(const PWildcard &p_wildcard) { oss << "_"; }

  void operator()(const Var &var) { oss << var.name; }

  void operator()(const IntConst &int_const) { oss << int_const.value; }

  std::string str(const Expr &expr) {
    std::visit(*this, expr);
    std::string res = oss.str();
    oss.str("");
    return res;
  }

  std::string str(const Pattern &pattern) {
    std::visit(*this, pattern);
    std::string res = oss.str();
    oss.str("");
    return res;
  }

  static std::string Print(const Expr &expr) { return Printer{}.str(expr); }

  static std::string Print(const Pattern &pattern) {
    return Printer{}.str(pattern);
  }

private:
  std::ostringstream oss;
};

// Expr equality
struct ExprEq {
  bool operator()(const ECall &lhs) {
    if (!std::holds_alternative<ECall>(rhs)) {
      return false;
    }
    const ECall &rhs_e_call = std::get<ECall>(rhs);
    if (lhs.fn != rhs_e_call.fn) {
      return false;
    }
    if (lhs.args.size() != rhs_e_call.args.size()) {
      return false;
    }
    for (size_t i = 0; i < lhs.args.size(); ++i) {
      if (!std::visit(ExprEq{rhs_e_call.args[i]}, lhs.args[i])) {
        return false;
      }
    }
    return true;
  }

  // TODO(@altanh): do we care about alpha equivalence?
  bool operator()(const Var &lhs) {
    if (!std::holds_alternative<Var>(rhs)) {
      return false;
    }
    const Var &rhs_var = std::get<Var>(rhs);
    return lhs.name == rhs_var.name;
  }

  bool operator()(const IntConst &lhs) {
    if (!std::holds_alternative<IntConst>(rhs)) {
      return false;
    }
    const IntConst &rhs_int_const = std::get<IntConst>(rhs);
    return lhs.value == rhs_int_const.value;
  }

  ExprEq(const Expr &rhs) : rhs(rhs) {}

  static bool Check(const Expr &lhs, const Expr &rhs) {
    return std::visit(ExprEq{rhs}, lhs);
  }

private:
  const Expr &rhs;
};

// Example: check if Expr has free variables

struct HasFreeVars {
  bool operator()(const Var &var) const { return true; }
  bool operator()(const IntConst &int_const) const { return false; }
  bool operator()(const ECall &e_call) const {
    bool res = false;
    for (const Expr &arg : e_call.args) {
      res |= std::visit(*this, arg);
    }
    return res;
  }

  static bool Check(const Expr &expr) {
    return std::visit(HasFreeVars{}, expr);
  }
};

} // namespace isle
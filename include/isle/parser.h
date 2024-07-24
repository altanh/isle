#pragma once

#include <iostream>
#include <memory>
#include <sstream>
#include <variant>

#include <isle/ir.h>

namespace isle {

// sexpr ::= int
//         | ident
//         | '(' sexpr* ')'

using SInt = int;
using SIdent = std::string;
struct SList;

using SExpr = std::variant<SInt, SIdent, SList>;

struct SList : std::vector<SExpr> {};

std::istream &operator>>(std::istream &is, SExpr &sexpr) {
  char c;
  is >> std::ws >> c;

  if (is.eof()) {
    std::cerr << "unexpected eof while parsing sexpr" << std::endl;
    is.setstate(std::ios_base::failbit);
    return is;
  }

  if (c == '(') {
    SList exprs;
    while (is.peek() != ')') {
      // check eof
      if (is.peek() == EOF) {
        std::cerr << "error: unexpected eof while parsing sexpr, expected ')'"
                  << std::endl;
        is.setstate(std::ios_base::failbit);
        return is;
      }
      SExpr expr;
      is >> expr;
      // check for errors
      if (is.fail()) {
        return is;
      }
      exprs.push_back(expr);
      // skip whitespace
      is >> std::ws;
    }
    is >> c;
    sexpr = exprs;
  } else if (std::isdigit(c)) {
    is.putback(c);
    int i;
    is >> i;
    // check for errors
    if (is.peek() != ')' && !std::isspace(is.peek())) {
      std::cerr << "error: expected int, got something else" << std::endl;
      is.setstate(std::ios_base::failbit);
      return is;
    }
    sexpr = i;
  } else if (c != ')') {
    std::ostringstream ident;
    ident << c;
    while (is.peek() != ')' && !std::isspace(is.peek()) && is.peek() != EOF) {
      ident << static_cast<char>(is.get());
    }
    sexpr = ident.str();
  } else {
    std::cerr << "error: unexpected ')' while parsing sexpr" << std::endl;
    is.setstate(std::ios_base::failbit);
  }
  return is;
}

std::ostream &operator<<(std::ostream &os, const SExpr &sexpr) {
  if (std::holds_alternative<int>(sexpr)) {
    os << std::get<SInt>(sexpr);
  } else if (std::holds_alternative<SIdent>(sexpr)) {
    os << std::get<SIdent>(sexpr);
  } else {
    const SList &exprs = std::get<SList>(sexpr);
    os << "(";
    if (!exprs.empty()) {
      os << exprs[0];
      for (size_t i = 1; i < exprs.size(); ++i) {
        os << " " << exprs[i];
      }
    }
    os << ")";
  }
  return os;
}

} // namespace isle

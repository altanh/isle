#pragma once

#include <memory>
#include <sstream>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

namespace isle {

using Id = int;
using Type = int;

struct FnDecl {
  Id id;
  std::string name;
  bool external;
  std::vector<Type> arg_types;
  Type ret_type;

  FnDecl(Id id, std::string name, bool external, std::vector<Type> arg_types,
         Type ret_type)
      : id(id), name(name), external(external), arg_types(arg_types),
        ret_type(ret_type) {}
};

struct ECall;
struct PCall;
struct Var;
struct IntConst;
struct PWildcard;

using Pattern = std::variant<PCall, PWildcard, Var, IntConst>;
using Expr = std::variant<ECall, Var, IntConst>;

struct ECall {
  Id fn;
  std::vector<Expr> args;

  ECall(Id fn, std::vector<Expr> args) : fn(fn), args(args) {}
};

struct PCall {
  Id fn;
  std::vector<Pattern> args;

  PCall(Id fn, std::vector<Pattern> args) : fn(fn), args(args) {}
};

struct PWildcard {};

struct Var {
  std::string name;

  Var(std::string name) : name(name) {}
};

struct IntConst {
  int value;

  IntConst(int value) : value(value) {}
};

/// Rule

struct Rule {
  Pattern pattern;
  Expr expr;
};

/// Generic visitor

template <class... Ts> struct Visitor : Ts... {
  using Ts::operator()...;
};

template <class... Ts> Visitor(Ts...) -> Visitor<Ts...>;

} // namespace isle
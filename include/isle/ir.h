#pragma once

#include <memory>
#include <optional>
#include <sstream>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

namespace isle {

using Id = int;

struct Var;
struct IntConst;
struct PCall;
struct PBind;
struct PWildcard;
struct ECall;

using Pattern = std::variant<PCall, PBind, PWildcard, Var, IntConst>;
using Expr = std::variant<ECall, Var, IntConst>;
using PatternRef = std::shared_ptr<Pattern>;
using ExprRef = std::shared_ptr<Expr>;

struct ECall {
  Id fn;
  std::vector<ExprRef> args;

  ECall(Id fn, std::vector<ExprRef> args) : fn(fn), args(args) {}

  ExprRef Clone() const { return std::make_shared<Expr>(*this); }
};

struct PCall {
  Id fn;
  std::vector<PatternRef> args;

  PCall(Id fn, std::vector<PatternRef> args) : fn(fn), args(args) {}

  PatternRef Clone() const { return std::make_shared<Pattern>(*this); }
};

struct PWildcard {};

namespace detail {

template <class T> struct CloneProxy {
  const T &v;
  CloneProxy(const T &v) : v(v) {}

  operator PatternRef() const { return std::make_shared<Pattern>(v); }

  operator ExprRef() const { return std::make_shared<Expr>(v); }
};

} // namespace detail

struct Var {
  std::string name;

  Var(std::string name) : name(name) {}

  detail::CloneProxy<Var> Clone() const {
    return detail::CloneProxy<Var>(*this);
  }
};

struct PBind {
  Var var;
  PatternRef pattern;

  PBind(Var var, PatternRef pattern) : var(var), pattern(pattern) {}

  PatternRef Clone() const { return std::make_shared<Pattern>(*this); }
};

struct IntConst {
  int value;

  IntConst(int value) : value(value) {}

  detail::CloneProxy<IntConst> Clone() const {
    return detail::CloneProxy<IntConst>(*this);
  }
};

/// Rule

struct Rule {
  Pattern pattern;
  Expr expr;

  Rule(Pattern pattern, Expr expr) : pattern(pattern), expr(expr) {}
};

/// Isle Program

struct EnumOption {
  std::string name;
  std::vector<std::pair<std::string, Id>> fields;
};

using Enum = std::vector<EnumOption>;

struct TypeDecl {
  Id id;
  std::string name;
  enum Kind {
    Internal,
    External,
    Primitive,
  } kind;
  // TODO(@altanh): merge with kind and make variant
  std::optional<Enum> options;
};

struct FnDecl {
  Id id;
  std::string name;
  bool external;
  std::optional<std::string> ctor;
  std::optional<std::string> xtor;
  std::vector<Id> arg_types;
  Id ret_type;
};

std::ostream &operator<<(std::ostream &, const Pattern &);
std::ostream &operator<<(std::ostream &, const Expr &);

struct Program {
  std::unordered_map<Id, std::string> type_names;
  std::unordered_map<std::string, Id> type_ids;
  std::unordered_map<Id, std::string> fn_names;
  std::unordered_map<std::string, Id> fn_ids;

  std::vector<TypeDecl> type_decls; // indexed by Id
  std::vector<FnDecl> fn_decls;     // indexed by Id
  std::vector<Rule> rules;

  void Print() const {
    std::cout << "declared types:\n";
    for (int i = 0; i < type_decls.size(); ++i) {
      const auto &ty = type_decls[i];
      std::cout << "\t" << i << ": " << ty.id << " " << ty.name << " "
                << ty.kind << "\n";
      // TODO print enum
      if (ty.kind != TypeDecl::Kind::Primitive) {
        for (const auto &opt : ty.options.value()) {
          std::cout << "\t\t" << opt.name << "\n";
          for (const auto &field : opt.fields) {
            std::cout << "\t\t\t" << field.first << ": " << field.second
                      << std::endl;
          }
        }
      }
    }
    std::cout << "declared funcs:\n";
    for (int i = 0; i < fn_decls.size(); ++i) {
      const auto &fn = fn_decls[i];
      std::cout << "\t" << i << ": " << fn.id << " " << fn.name << " "
                << fn.external << " " << fn.ctor.value_or("n/a") << " "
                << fn.xtor.value_or("n/a") << " (";
      if (!fn.arg_types.empty()) {
        std::cout << fn.arg_types[0];
        for (int j = 1; j < fn.arg_types.size(); ++j) {
          std::cout << " " << fn.arg_types[j];
        }
      }
      std::cout << ") " << fn.ret_type << "\n";
    }
    std::cout << "rules:\n";
    for (const auto &rule : rules) {
      std::cout << "\t" << rule.pattern << " -> " << rule.expr << "\n";
    }
    std::cout << std::flush;
  }
};

// TODO(@altanh): typechecking

/// Generic visitor

template <class... Ts> struct Visitor : Ts... {
  using Ts::operator()...;
};

template <class... Ts> Visitor(Ts...) -> Visitor<Ts...>;

} // namespace isle
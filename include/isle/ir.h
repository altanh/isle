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
struct PAnd;
struct PWildcard;
struct ECall;
struct ELet;

using Pattern = std::variant<PCall, PAnd, PBind, PWildcard, Var, IntConst>;
using Expr = std::variant<ECall, ELet, Var, IntConst>;
using PatternRef = std::shared_ptr<Pattern>;
using ExprRef = std::shared_ptr<Expr>;

struct Var {
  std::string name;

  Var(std::string name) : name(name) {}

  operator PatternRef() const { return std::make_shared<Pattern>(*this); }

  operator ExprRef() const { return std::make_shared<Expr>(*this); }

  operator const std::string &() const { return name; }
};

struct ECall {
  Id fn;
  std::vector<ExprRef> args;

  ECall(Id fn, const std::vector<ExprRef> &args) : fn(fn), args(args) {}

  operator ExprRef() const { return std::make_shared<Expr>(*this); }
};

struct ELet {
  Var var;
  ExprRef value;
  ExprRef body;

  ELet(Var var, ExprRef value, ExprRef body)
      : var(var), value(value), body(body) {}

  operator ExprRef() const { return std::make_shared<Expr>(*this); }
};

struct PCall {
  Id fn;
  std::vector<PatternRef> args;

  PCall(Id fn, const std::vector<PatternRef> &args) : fn(fn), args(args) {}

  operator PatternRef() const { return std::make_shared<Pattern>(*this); }
};

struct PAnd {
  std::vector<PatternRef> patterns;

  PAnd(const std::vector<PatternRef> &patterns) : patterns(patterns) {}

  operator PatternRef() const { return std::make_shared<Pattern>(*this); }
};

struct PWildcard {
  operator PatternRef() const { return std::make_shared<Pattern>(*this); }
};

struct PBind {
  Var var;
  PatternRef pattern;

  PBind(Var var, PatternRef pattern) : var(var), pattern(pattern) {}

  operator PatternRef() const { return std::make_shared<Pattern>(*this); }
};

struct IntConst {
  int value;

  IntConst(int value) : value(value) {}

  operator ExprRef() const { return std::make_shared<Expr>(*this); }
  operator PatternRef() const { return std::make_shared<Pattern>(*this); }
  operator int() const { return value; }
};

/// Rule

struct Rule {
  Pattern pattern;
  Expr expr;
  int priority;

  Rule(Pattern pattern, Expr expr, int priority)
      : pattern(pattern), expr(expr), priority(priority) {}

  Rule(Pattern pattern, Expr expr)
      : pattern(pattern), expr(expr), priority(0) {}
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
  std::unordered_map<Id, std::vector<Rule>> fn_rules;

  void Print() const;
};

// TODO(@altanh): typechecking

/// Generic visitor

template <class... Ts> struct Visitor : Ts... {
  using Ts::operator()...;
};

template <class... Ts> Visitor(Ts...) -> Visitor<Ts...>;

class Printer {
  const Program *prog_;
  std::ostream &os_;

public:
  Printer(std::ostream &os, const Program *prog = nullptr)
      : os_(os), prog_(prog) {}

  Printer &operator<<(const Var &var) {
    os_ << var.name;
    return *this;
  }

  Printer &operator<<(const IntConst &i) {
    os_ << i.value;
    return *this;
  }

  Printer &operator<<(const Expr &e) {
    std::visit(*this, e);
    return *this;
  }

  Printer &operator<<(const Pattern &p) {
    std::visit(*this, p);
    return *this;
  }

  template <typename T> Printer &operator<<(const T &t) {
    os_ << t;
    return *this;
  }

  void operator()(const ECall &ecall) {
    *this << "(";
    if (prog_) {
      *this << prog_->fn_names.at(ecall.fn);
    } else {
      *this << "%" << ecall.fn;
    }
    for (const auto &arg : ecall.args) {
      *this << " " << *arg;
    }
    *this << ")";
  }

  void operator()(const ELet &elet) {
    *this << "(let (";
    *this << "(" << elet.var << " " << *elet.value << ")";
    ExprRef e = elet.body;
    while (const ELet *l = std::get_if<ELet>(e.get())) {
      *this << " (" << l->var << " " << *l->value << ")";
      e = l->body;
    }
    *this << ") " << *e << ")";
  }

  void operator()(const PCall &pcall) {
    *this << "(";
    if (prog_) {
      *this << prog_->fn_names.at(pcall.fn);
    } else {
      *this << "%" << pcall.fn;
    }
    for (const auto &arg : pcall.args) {
      *this << " " << *arg;
    }
    *this << ")";
  }

  void operator()(const PAnd &pand) {
    *this << "(and";
    for (const auto &pat : pand.patterns) {
      *this << " " << *pat;
    }
    *this << ")";
  }

  void operator()(const PBind &pbind) {
    *this << "(@ " << pbind.var << " " << *pbind.pattern << ")";
  }

  void operator()(const PWildcard &pwildcard) { *this << "_"; }

  void operator()(const Var &var) { *this << var.name; }

  void operator()(const IntConst &i) { *this << i.value; }
};

std::ostream &operator<<(std::ostream &os, const Var &var) {
  os << var.name;
  return os;
}

std::ostream &operator<<(std::ostream &os, const IntConst &i) {
  os << i.value;
  return os;
}

std::ostream &operator<<(std::ostream &os, const Expr &expr) {
  Printer(os) << expr;
  return os;
}

std::ostream &operator<<(std::ostream &os, const Pattern &pattern) {
  Printer(os) << pattern;
  return os;
}

void Program::Print() const {
  std::cout << "declared types:\n";
  for (int i = 0; i < type_decls.size(); ++i) {
    const auto &ty = type_decls[i];
    std::cout << "\t" << i << ": " << ty.id << " " << ty.name << " " << ty.kind
              << "\n";
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

  Printer pp(std::cout, this);

  for (const auto &pr : fn_rules) {
    for (const auto &rule : pr.second) {
      pp << "\t[" << rule.priority << "] " << rule.pattern << " -> "
         << rule.expr << "\n";
    }
  }
  std::cout << std::flush;
}

} // namespace isle
#pragma once

#include <memory>
#include <string>
#include <vector>

namespace isle {

using Id = int;
using Type = int;
using std::string;
template <typename T> using Ref = std::shared_ptr<T>;

struct FnDecl {
  Id id;
  string name;
  std::vector<Type> arg_types;
  Type ret_type;
};

// Helper to make Ref
#define MAKE_REF(T)                                                            \
  template <typename... Args> static Ref<T> Make(Args &&...args) {             \
    return std::make_shared<T>(std::forward<Args>(args)...);                   \
  }

/// Visitors

struct ECall;
struct Var;
struct IntConst;

struct ExprVisitor {
  virtual void Visit(ECall *expr) = 0;
  virtual void Visit(Var *expr) = 0;
  virtual void Visit(IntConst *expr) = 0;
};

struct PCall;
struct PWildcard;

struct PatternVisitor {
  virtual void Visit(PCall *pattern) = 0;
  virtual void Visit(PWildcard *pattern) = 0;
  virtual void Visit(Var *pattern) = 0;
  virtual void Visit(IntConst *pattern) = 0;
};

/// Expr

struct Expr {
  virtual void Accept(ExprVisitor *visitor) = 0;
};

struct ECall : Expr {
  Id fn;
  std::vector<Ref<Expr>> args;

  ECall(Id fn, std::vector<Ref<Expr>> args) : fn(fn), args(args) {}

  // variadic constructor
  template <typename... Args>
  ECall(Id fn, Args &&...args) : fn(fn), args{std::forward<Args>(args)...} {}

  void Accept(ExprVisitor *visitor) override { visitor->Visit(this); }

  MAKE_REF(ECall);
};

/// Pattern

struct Pattern {
  virtual void Accept(PatternVisitor *visitor) = 0;
};

struct PWildcard : Pattern {
  PWildcard() {}

  void Accept(PatternVisitor *visitor) override { visitor->Visit(this); }

  MAKE_REF(PWildcard);
};

struct PCall : Pattern {
  Id fn;
  std::vector<Ref<Pattern>> args;

  PCall(Id fn, std::vector<Ref<Pattern>> args) : fn(fn), args(args) {}

  // variadic constructor
  template <typename... Args>
  PCall(Id fn, Args &&...args) : fn(fn), args{std::forward<Args>(args)...} {}

  void Accept(PatternVisitor *visitor) override { visitor->Visit(this); }

  MAKE_REF(PCall);
};

/// Shared terms

struct Var : Expr, Pattern {
  string name;

  Var(string name) : name(name) {}

  void Accept(ExprVisitor *visitor) override { visitor->Visit(this); }
  void Accept(PatternVisitor *visitor) override { visitor->Visit(this); }

  MAKE_REF(Var);
};

struct IntConst : Expr, Pattern {
  int value;

  IntConst(int value) : value(value) {}

  void Accept(ExprVisitor *visitor) override { visitor->Visit(this); }
  void Accept(PatternVisitor *visitor) override { visitor->Visit(this); }

  MAKE_REF(IntConst);
};

/// Rule

struct Rule {
  Ref<Pattern> pattern;
  Ref<Expr> expr;

  Rule(Ref<Pattern> pattern, Ref<Expr> expr) : pattern(pattern), expr(expr) {}

  MAKE_REF(Rule);
};

} // namespace isle

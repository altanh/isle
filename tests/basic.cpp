#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

#include <isle/ir.h>

/*
(decl + (Int Int) Int)
(decl * (Int Int) Int)
(decl << (Int Int) Int)

(rule (+ x x) (* x 2))
(rule (* x 2) (<< x 1))
(rule (+ x 0) x)
(rule (* x 1) x)
(rule (* x 0) 0)
*/

// how does isle avoid cyclic rewrites? x -> y -> x -> ...
// is it just via careful priorities?

using TypeEnv = std::unordered_map<std::string, isle::Type>;
using DeclEnv = std::unordered_map<isle::Id, isle::FnDecl>;

isle::Type RegisterType(TypeEnv *env, const std::string &name) {
  auto it = env->find(name);
  if (it != env->end()) {
    return it->second;
  }
  isle::Type id = env->size();
  (*env)[name] = id;
  return id;
}

isle::Id DeclareFn(DeclEnv *env, const std::string &name,
                   const std::vector<isle::Type> &arg_types,
                   isle::Type ret_type) {
  // auto it = env->find(name);
  // if (it != env->end()) {
  //   return it->second.id;
  // }
  // TODO: do I want another table for looking up by name?
  isle::FnDecl decl = {env->size(), name, arg_types, ret_type};
  (*env)[decl.id] = decl;
  return decl.id;
}

void PrintEnv(const TypeEnv &type_env, const DeclEnv &decl_env) {
  std::cout << "TypeEnv:\n";
  for (const auto &pair : type_env) {
    std::cout << "\t" << pair.first << ": " << pair.second << "\n";
  }
  std::cout << "DeclEnv:\n";
  for (const auto &pair : decl_env) {
    const isle::FnDecl &decl = pair.second;
    std::cout << "\t" << decl.name << " ( ";
    for (size_t i = 0; i < decl.arg_types.size(); ++i) {
      std::cout << decl.arg_types[i] << " ";
    }
    std::cout << ") -> " << decl.ret_type << ": " << decl.id << "\n";
  }
  std::cout << std::flush;
}

struct PatternPrinter : isle::PatternVisitor {
  PatternPrinter(const DeclEnv &decl_env) : decl_env(decl_env) {}

  void Visit(isle::PCall *pattern) override {
    std::cout << "(" << decl_env.at(pattern->fn).name;
    for (const isle::Ref<isle::Pattern> &arg : pattern->args) {
      std::cout << " ";
      arg->Accept(this);
    }
    std::cout << ")";
  }

  void Visit(isle::PWildcard *pattern) override { std::cout << "_"; }

  void Visit(isle::Var *pattern) override { std::cout << pattern->name; }

  void Visit(isle::IntConst *pattern) override { std::cout << pattern->value; }

private:
  const DeclEnv &decl_env;
};

struct ExprPrinter : isle::ExprVisitor {
  ExprPrinter(const DeclEnv &decl_env) : decl_env(decl_env) {}

  void Visit(isle::ECall *expr) override {
    std::cout << "(" << decl_env.at(expr->fn).name;
    for (const isle::Ref<isle::Expr> &arg : expr->args) {
      std::cout << " ";
      arg->Accept(this);
    }
    std::cout << ")";
  }

  void Visit(isle::Var *expr) override { std::cout << expr->name; }

  void Visit(isle::IntConst *expr) override { std::cout << expr->value; }

private:
  const DeclEnv &decl_env;
};

void PrintRules(const std::vector<isle::Rule> &rules, const DeclEnv &decl_env) {
  PatternPrinter pp(decl_env);
  ExprPrinter ep(decl_env);

  std::cout << "Rules:\n";
  for (const isle::Rule &rule : rules) {
    std::cout << "\t";
    rule.pattern->Accept(&pp);
    std::cout << " -> ";
    rule.expr->Accept(&ep);
    std::cout << "\n";
  }
  std::cout << std::flush;
}

int main(int argc, char **argv) {
  using std::string;
  using std::unordered_map;
  using std::vector;
  using namespace isle;

  TypeEnv type_env;
  DeclEnv decl_env;

  Type int_type = RegisterType(&type_env, "Int");

  Id plus = DeclareFn(&decl_env, "+", {int_type, int_type}, int_type);
  Id times = DeclareFn(&decl_env, "*", {int_type, int_type}, int_type);
  Id shl = DeclareFn(&decl_env, "<<", {int_type, int_type}, int_type);

  vector<Rule> rules;

  Ref<Var> x = Var::Make("x");
  Ref<IntConst> zero = IntConst::Make(0);
  Ref<IntConst> one = IntConst::Make(1);
  Ref<IntConst> two = IntConst::Make(2);

  rules.push_back(Rule{PCall::Make(plus, vector<Ref<Pattern>>{x, x}),
                       ECall::Make(times, x, two)});

  rules.push_back(Rule{PCall::Make(times, x, two), ECall::Make(shl, x, one)});
  rules.push_back(Rule{PCall::Make(plus, x, zero), x});
  rules.push_back(Rule{PCall::Make(times, x, one), x});
  rules.push_back(Rule{PCall::Make(times, x, zero), zero});

  PrintEnv(type_env, decl_env);
  PrintRules(rules, decl_env);

  return 0;
}

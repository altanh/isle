#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

#include <isle/ir.h>
#include <isle/parser.h>
#include <isle/visitors.h>

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

// using TypeEnv = std::unordered_map<std::string, isle::Type>;
// using DeclEnv = std::unordered_map<isle::Id, isle::FnDecl>;

// isle::Type RegisterType(TypeEnv *env, const std::string &name) {
//   auto it = env->find(name);
//   if (it != env->end()) {
//     return it->second;
//   }
//   isle::Type id = env->size();
//   (*env)[name] = id;
//   return id;
// }

// isle::Id DeclareFn(DeclEnv *env, const std::string &name, bool external,
//                    const std::vector<isle::Type> &arg_types,
//                    isle::Type ret_type) {
//   // auto it = env->find(name);
//   // if (it != env->end()) {
//   //   return it->second.id;
//   // }
//   // TODO: do I want another table for looking up by name?
//   isle::Id id = env->size();
//   env->insert({id, isle::FnDecl{id, name, external, arg_types, ret_type}});
//   return id;
// }

// void PrintEnv(const TypeEnv &type_env, const DeclEnv &decl_env) {
//   std::cout << "TypeEnv:\n";
//   for (const auto &pair : type_env) {
//     std::cout << "\t" << pair.first << ": " << pair.second << "\n";
//   }
//   std::cout << "DeclEnv:\n";
//   for (const auto &pair : decl_env) {
//     const isle::FnDecl &decl = pair.second;
//     std::cout << "\t" << decl.name << " ( ";
//     for (size_t i = 0; i < decl.arg_types.size(); ++i) {
//       std::cout << decl.arg_types[i] << " ";
//     }
//     std::cout << ") -> " << decl.ret_type << ": " << decl.id << "\n";
//   }
//   std::cout << std::flush;
// }

// void PrintRules(const std::vector<isle::Rule> &rules, const DeclEnv
// &decl_env) {
//   std::cout << "Rules:\n";
//   for (const isle::Rule &rule : rules) {
//     // std::cout << "\t" << isle::PatternPrinter(decl_env).str(rule.pattern)
//     //           << " -> " << isle::ExprPrinter(decl_env).str(rule.expr) <<
//     //           "\n";
//     std::cout << "\t" << isle::Printer::Print(rule.pattern) << " -> "
//               << isle::Printer::Print(rule.expr) << "\n";
//   }
//   std::cout << std::flush;
// }

int main(int argc, char **argv) {
  using std::string;
  using std::unordered_map;
  using std::vector;
  using namespace isle;

  // TypeEnv type_env;
  // DeclEnv decl_env;

  // Type int_type = RegisterType(&type_env, "Int");

  // Id plus = DeclareFn(&decl_env, "+", false, {int_type, int_type}, int_type);
  // Id times = DeclareFn(&decl_env, "*", false, {int_type, int_type},
  // int_type); Id shl = DeclareFn(&decl_env, "<<", false, {int_type, int_type},
  // int_type);

  // vector<Rule> rules;

  // Var x("x");
  // IntConst zero(0);
  // IntConst one(1);
  // IntConst two(2);

  // rules.push_back(Rule{PCall(plus, {x, x}), ECall(times, {x, two})});
  // rules.push_back(Rule{PCall(times, {x, two}), ECall(shl, {x, one})});
  // rules.push_back(Rule{PCall(plus, {x, zero}), x});
  // rules.push_back(Rule{PCall(times, {x, one}), x});
  // rules.push_back(Rule{PCall(times, {x, zero}), zero});

  // PrintEnv(type_env, decl_env);
  // PrintRules(rules, decl_env);

  return 0;
}

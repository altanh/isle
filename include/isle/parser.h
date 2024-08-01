#pragma once

#include <iostream>
#include <memory>
#include <optional>
#include <sstream>
#include <unordered_map>
#include <variant>
#include <vector>

#include "ir.h"
#include "visitors.h"

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
    // std::cerr << "unexpected eof while parsing sexpr" << std::endl;
    // is.setstate(std::ios_base::failbit);
    return is;
  }

  if (c == ';') {
    // skip until end of line
    std::string tmp;
    std::getline(is, tmp, '\n');
    return (is >> sexpr);
  } else if (c == '(') {
    SList exprs;
    while (is.peek() != ')') {
      // check eof
      if (is.peek() == EOF) {
        std::cerr << "error: unexpected eof while parsing sexpr, expected ')'"
                  << std::endl;
        is.setstate(std::ios_base::badbit);
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
      is.setstate(std::ios_base::badbit);
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
    is.setstate(std::ios_base::badbit);
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

/**
 * stmt ::= decl-type
 *        | decl-fn
 *        | ctor
 *        | xtor
 *        | rule
 *
 * decl-type ::= '(' 'type' ident ('extern' | 'primitive')? enum? ')'
 *
 * enum ::= '(' 'enum' ( '(' ident ( '('ident ident ')' )* ')' )+ ')'
 *        | '(' 'enum' ident+ ')'
 *
 * decl-fn ::= '(' 'decl' 'extern'? ident '(' ident* ')' ident ')'
 *
 * ctor ::= '(' 'constructor' ident string ')'
 *
 * xtor ::= '(' 'extractor' ident string ')'
 *
 * rule ::= '(' 'rule' root-pattern root-expr ')'
 *
 * root-pattern ::= '(' ident pattern* ')'
 *
 * pattern ::= '(' ident pattern* ')'
 *           | ident
 *           | int
 *           | '_'
 *
 * root-expr ::= '(' ident expr* ')'
 *
 * expr ::= '(' ident expr* ')'
 *        | ident
 *        | int
 *
 * program ::= stmt*
 */

std::optional<EnumOption> ParseEnumOption(const Program &program,
                                          const SExpr &sexpr) {
  const SList *opt = std::get_if<SList>(&sexpr);
  if (!opt) {
    std::cerr << "error: expected enum option definition: " << sexpr
              << std::endl;
    return {};
  }

  if (opt->size() < 1 || !std::holds_alternative<SIdent>(opt->front())) {
    std::cerr << "error: expected identifier for enum option name: " << sexpr
              << std::endl;
    return {};
  }

  EnumOption result;
  result.name = std::get<SIdent>(opt->front());
  // parse the fields
  for (int i = 1; i < opt->size(); ++i) {
    const SList *field = std::get_if<SList>(&(*opt)[i]);
    if (!field || field->size() != 2 ||
        !std::holds_alternative<SIdent>(field->front()) ||
        !std::holds_alternative<SIdent>(field->back())) {
      std::cerr << "error: expected field definition: " << (*opt)[i]
                << std::endl;
      return {};
    }
    const auto &type_name = std::get<SIdent>(field->back());
    auto it = program.type_ids.find(type_name);
    if (it == program.type_ids.end()) {
      std::cerr << "error: undeclared type \"" << type_name
                << "\" in enum definition: " << sexpr << std::endl;
      return {};
    }
    result.fields.emplace_back(std::get<SIdent>(field->front()), it->second);
  }

  return result;
}

std::optional<Enum> ParseEnum(const Program &program, const SExpr &sexpr) {
  const SList *enum_ = std::get_if<SList>(&sexpr);
  if (!enum_) {
    std::cerr << "error: expected enum definition: " << sexpr << std::endl;
    return {};
  }
  if (enum_->size() < 1 || !std::holds_alternative<SIdent>(enum_->front()) ||
      std::get<SIdent>(enum_->front()) != "enum") {
    std::cerr << "error: expected \"enum\" in enum definition: " << sexpr
              << std::endl;
    return {};
  }
  // (1) flat list of identifiers
  // (2) list of enum options
  if (enum_->size() < 2) {
    std::cerr << "error: expected at least one variant in enum definition: "
              << sexpr << std::endl;
    return {};
  }
  Enum result;
  if (std::holds_alternative<SIdent>((*enum_)[1])) {
    // case (1)
    for (int i = 1; i < enum_->size(); ++i) {
      const SIdent *option_name = std::get_if<SIdent>(&(*enum_)[i]);
      if (!option_name) {
        std::cerr << "error: expected identifier for option name: " << sexpr
                  << std::endl;
        return {};
      }
      result.emplace_back(
          EnumOption{*option_name, std::vector<std::pair<std::string, Id>>{}});
    }
  } else {
    // case (2)
    for (int i = 1; i < enum_->size(); ++i) {
      auto opt = ParseEnumOption(program, (*enum_)[i]);
      if (!opt) {
        return {};
      }
      result.push_back(opt.value());
    }
  }
  return result;
}

std::optional<TypeDecl> ParseTypeDecl(const SList &stmt, Program *program) {
  TypeDecl type_decl;
  type_decl.id = program->type_ids.size();

  if (stmt.size() < 2) {
    std::cerr << "error: expected identifier in type declaration: "
              << static_cast<SExpr>(stmt) << std::endl;
    return {};
  }
  const SIdent *ident = std::get_if<SIdent>(&stmt[1]);
  if (!ident) {
    std::cerr << "error: expected identifier in type declaration: " << stmt
              << std::endl;
    return {};
  }
  // check that the type hasn't already been declared
  if (program->type_ids.count(*ident)) {
    std::cerr << "error: type has already been declared: " << stmt << std::endl;
    return {};
  }
  type_decl.name = *ident;

  if (stmt.size() < 3) {
    std::cerr << "error: expected type to be an enum or a primitive: " << stmt
              << std::endl;
    return {};
  }
  ident = std::get_if<SIdent>(&stmt[2]);

  if (!ident) {
    // try to parse internal enum
    type_decl.kind = TypeDecl::Kind::Internal;
    type_decl.options = ParseEnum(*program, stmt[2]);
    if (!type_decl.options) {
      return {};
    }
  } else if (*ident == "extern") {
    type_decl.kind = TypeDecl::Kind::External;
    if (stmt.size() != 4) {
      std::cerr << "error: missing enum definition: " << stmt << std::endl;
      return {};
    }
    type_decl.options = ParseEnum(*program, stmt[3]);
    if (!type_decl.options) {
      return {};
    }
  } else if (*ident == "primitive") {
    type_decl.kind = TypeDecl::Kind::Primitive;
    type_decl.options = {};
  } else {
    std::cerr << "error: unexpected identifier \"" << *ident << "\": " << stmt
              << std::endl;
    return {};
  }

  program->type_names.insert({type_decl.id, type_decl.name});
  program->type_ids.insert({type_decl.name, type_decl.id});

  return type_decl;
}

std::optional<FnDecl> ParseFnDecl(const SList &stmt, Program *program) {
  if (stmt.size() < 4) {
    std::cerr << "error: function declaration must have name, argument types, "
                 "and return type: "
              << stmt << std::endl;
    return {};
  }

  FnDecl result;
  result.id = program->fn_decls.size();
  result.external = false;

  int i = 1;

  const SIdent *ident = std::get_if<SIdent>(&stmt[i]);
  if (!ident) {
    std::cerr << "error: expected identifier: " << stmt << std::endl;
    return {};
  }
  if (*ident == "extern") {
    result.external = true;
    if (stmt.size() != 5) {
      std::cerr
          << "error: function declaration must have name, argument types, "
             "and return type: "
          << stmt << std::endl;
      return {};
    }
    ++i;
    ident = std::get_if<SIdent>(&stmt[i]);
    if (!ident) {
      std::cerr << "error: expected identifier: " << stmt << std::endl;
      return {};
    }
  }
  result.name = *ident;

  // parse argument types
  ++i;
  const SList *arg_types = std::get_if<SList>(&stmt[i]);
  if (!arg_types) {
    std::cerr << "error: expected list of argument types: " << stmt
              << std::endl;
    return {};
  }
  for (int j = 0; j < arg_types->size(); ++j) {
    ident = std::get_if<SIdent>(&(*arg_types)[j]);
    if (!ident) {
      std::cerr << "error: expected identifier in argument types: " << stmt
                << std::endl;
      return {};
    }
    // lookup type
    if (!program->type_ids.count(*ident)) {
      // TODO(@altanh): should this be 2-phase to enable out of order parsing?
      std::cerr << "error: undeclared type \"" << *ident
                << "\" in function declaration: " << stmt << std::endl;
      return {};
    }
    result.arg_types.push_back(program->type_ids.at(*ident));
  }

  // parse return type
  ++i;
  ident = std::get_if<SIdent>(&stmt[i]);
  if (!ident) {
    std::cerr << "error: expected identifier in return type:" << stmt
              << std::endl;
    return {};
  }
  // lookup type
  if (!program->type_ids.count(*ident)) {
    // TODO(@altanh): should this be 2-phase to enable out of order parsing?
    std::cerr << "error: undeclared type \"" << *ident
              << "\" in function declaration: " << stmt << std::endl;
    return {};
  }
  result.ret_type = program->type_ids.at(*ident);

  program->fn_names.insert({result.id, result.name});
  program->fn_ids.insert({result.name, result.id});

  return result;
}

std::optional<std::string>
ParseExtern(const SList &stmt, const std::string &kind, Program *program) {
  if (stmt.size() != 3) {
    std::cerr << "error: expected function identifier and extern string: "
              << stmt << std::endl;
    return {};
  }
  const SIdent *ident = std::get_if<SIdent>(&stmt[1]);
  if (!ident) {
    std::cerr << "error: expected function identifier: " << stmt << std::endl;
    return {};
  }
  // lookup function
  auto it = program->fn_ids.find(*ident);
  if (it == program->fn_ids.end()) {
    std::cerr << "error: undeclared function in extern declaration: " << stmt
              << std::endl;
    return {};
  }
  // check function is extern
  auto &fn = program->fn_decls[it->second];
  if (!fn.external) {
    std::cerr
        << "error: function in extern declaration is not declared extern: "
        << stmt << std::endl;
    return {};
  }

  // parse the extern
  ident = std::get_if<SIdent>(&stmt[2]);
  if (!ident) {
    std::cerr << "error: expected string for extern declaration: " << stmt
              << std::endl;
    return {};
  }
  // very basic sanity check quotes
  if (ident->size() < 3 || ident->front() != '"' || ident->back() != '"') {
    std::cerr << "error: expected (nonempty) string for extern declaration: "
              << stmt << std::endl;
    return {};
  }
  std::string str = ident->substr(1, ident->size() - 2);

  // attach
  if (kind == "constructor") {
    // check not already declared
    if (fn.ctor) {
      std::cerr << "error: function \"" << fn.name
                << "\" has previously declared extern constructor \""
                << fn.ctor.value() << "\": " << stmt << std::endl;
      return {};
    }
    fn.ctor = str;
  } else {
    // check not already declared
    if (fn.xtor) {
      std::cerr << "error: function \"" << fn.name
                << "\" has previously declared extern extractor \""
                << fn.ctor.value() << "\": " << stmt << std::endl;
      return {};
    }
    fn.xtor = str;
  }

  return str;
}

std::optional<PatternRef> ParsePattern(const Program &program,
                                       const SExpr &sexpr, bool root) {
  return std::visit(
      Visitor{
          [&](const SList &list) -> std::optional<PatternRef> {
            if (list.size() < 1) {
              std::cerr << "error: empty list in pattern: " << sexpr
                        << std::endl;
              return {};
            }
            const SIdent *ident = std::get_if<SIdent>(&list[0]);
            if (!ident) {
              std::cerr
                  << "error: expected function identifier in pattern but got "
                  << list[0] << ": " << sexpr << std::endl;
              return {};
            }

            // special case: "@" for binding subpatterns
            if (*ident == "@") {
              if (root) {
                std::cerr << "error: expected call pattern at root, but got "
                          << sexpr << std::endl;
                return {};
              }
              // (@ var subpattern)
              if (list.size() != 3) {
                std::cerr << "error: expected variable and subpattern in "
                             "subpattern binding: "
                          << sexpr << std::endl;
                return {};
              }
              ident = std::get_if<SIdent>(&list[1]);
              if (!ident) {
                std::cerr << "error: expected variable identifier in "
                             "subpattern binding: "
                          << sexpr << std::endl;
                return {};
              }
              auto subpattern = ParsePattern(program, list[2], false);
              if (!subpattern) {
                return {};
              }
              return PBind(Var(*ident), *subpattern);
            } else if (*ident == "and") {
              if (list.size() < 2) {
                std::cerr << "error: and needs at least one pattern!" << sexpr
                          << std::endl;
                return {};
              }
              if (root) {
                std::cerr << "error: expected call pattern at root, but got "
                          << sexpr << std::endl;
                return {};
              }
              std::vector<PatternRef> subpatterns;
              for (int i = 1; i < list.size(); ++i) {
                auto subpattern = ParsePattern(program, list[i], false);
                if (!subpattern) {
                  return {};
                }
                subpatterns.emplace_back(*subpattern);
              }
              return PAnd(subpatterns);
            }

            // lookup function
            auto it = program.fn_ids.find(*ident);
            if (it == program.fn_ids.end()) {
              std::cerr << "error: undeclared function \"" << *ident
                        << "\" in pattern: " << sexpr << std::endl;
              return {};
            }
            PatternRef res = PCall(it->second, {});
            // parse children
            for (int i = 1; i < list.size(); ++i) {
              auto c = ParsePattern(program, list[i], false);
              if (!c) {
                return {};
              }
              std::get<PCall>(*res).args.push_back(*c);
            }
            return res;
          },
          [&](const SIdent &ident) -> std::optional<PatternRef> {
            if (root) {
              std::cerr << "error: expected call at pattern root, but got "
                        << sexpr << std::endl;
              return {};
            }
            if (ident == "_") {
              return PWildcard();
            }
            return Var(ident);
          },
          [&](const SInt &i) -> std::optional<PatternRef> {
            if (root) {
              std::cerr << "error: expected call at pattern root, but got "
                        << sexpr << std::endl;
              return {};
            }
            return IntConst(i);
          }},
      sexpr);
}

std::optional<ExprRef> ParseExpr(const Program &program, const SExpr &sexpr,
                                 bool root) {
  return std::visit(
      Visitor{[&](const SList &list) -> std::optional<ExprRef> {
                if (list.size() < 1) {
                  std::cerr << "error: empty list in expr: " << sexpr
                            << std::endl;
                  return {};
                }
                const SIdent *ident = std::get_if<SIdent>(&list[0]);
                if (!ident) {
                  std::cerr
                      << "error: expected function identifier in expr but got "
                      << list[0] << ": " << sexpr << std::endl;
                  return {};
                }
                // (let ((var value)...) body)
                if (*ident == "let") {
                  if (list.size() != 3) {
                    std::cerr
                        << "error: expected bindings and body in let binding: "
                        << sexpr << std::endl;
                    return {};
                  }
                  // NB: we conservatively disallow (let ((x (f ...)) x) in the
                  // pattern root, but allow e.g. (let ((x (f ...))) (g x x))
                  const SList *blist = std::get_if<SList>(&list[1]);
                  if (!blist) {
                    std::cerr << "error: expected a list of bindings: " << sexpr
                              << std::endl;
                    return {};
                  }
                  std::vector<std::pair<Var, ExprRef>> bindings;
                  for (const auto &b : *blist) {
                    const SList *binding = std::get_if<SList>(&b);
                    if (!binding || binding->size() != 2) {
                      std::cerr << "error: expected a pair of var and expr for "
                                   "binding, but got "
                                << b << ": " << sexpr << std::endl;
                      return {};
                    }
                    const auto var = ParseExpr(program, (*binding)[0], false);
                    const auto val = ParseExpr(program, (*binding)[1], false);
                    if (!var || !std::get_if<Var>(var->get())) {
                      std::cerr << "error: expected var in binding, but got "
                                << b << ": " << sexpr << std::endl;
                      return {};
                    }
                    if (!val) {
                      std::cerr
                          << "error: failed to parse binding value: " << sexpr
                          << std::endl;
                      return {};
                    }
                    bindings.emplace_back(std::get<Var>(**var), *val);
                  }
                  auto body = ParseExpr(program, list[2], root);
                  if (!body) {
                    std::cerr << "error: failed to parse let body: " << sexpr
                              << std::endl;
                    return {};
                  }
                  // build return expr by going through the bindings backwards
                  ExprRef result = *body;
                  for (int i = 0; i < bindings.size(); ++i) {
                    const int j = bindings.size() - i - 1;
                    result =
                        ELet(bindings[j].first, bindings[j].second, result);
                  }
                  return result;
                }
                // lookup function
                auto it = program.fn_ids.find(*ident);
                if (it == program.fn_ids.end()) {
                  std::cerr << "error: undeclared function \"" << *ident
                            << "\" in expr: " << sexpr << std::endl;
                  return {};
                }
                ECall result(it->second, {});
                // parse children
                for (int i = 1; i < list.size(); ++i) {
                  auto c = ParseExpr(program, list[i], false);
                  if (!c) {
                    return {};
                  }
                  result.args.push_back(*c);
                }
                return result;
              },
              [&](const SIdent &ident) -> std::optional<ExprRef> {
                if (root) {
                  std::cerr << "error: expected call at expr root, but got "
                            << sexpr << std::endl;
                  return {};
                }
                return Var(ident);
              },
              [&](const SInt &i) -> std::optional<ExprRef> {
                if (root) {
                  std::cerr << "error: expected call at expr root, but got "
                            << sexpr << std::endl;
                  return {};
                }
                return IntConst(i);
              }},
      sexpr);
}

std::optional<Rule> ParseRule(const Program &program, const SList &stmt) {
  if (stmt.size() < 3) {
    std::cerr << "error: expected pattern and expression in rule: " << stmt
              << std::endl;
    return {};
  }

  int priority = 0;
  int i = 0;
  if (std::holds_alternative<SInt>(stmt[1])) {
    if (stmt.size() != 4) {
      std::cerr << "error: expected pattern and expression in rule: " << stmt
                << std::endl;
      return {};
    }
    priority = std::get<SInt>(stmt[1]);
    i = 1;
  }
  auto pat = ParsePattern(program, stmt[i + 1], true);
  auto expr = ParseExpr(program, stmt[i + 2], true);

  if (!pat) {
    std::cerr << "error: failed to parse pattern" << std::endl;
    return {};
  }
  if (!expr) {
    std::cerr << "error: failed to parse expr" << std::endl;
    return {};
  }

  // check rule wellformedness
  std::unordered_map<std::string, int> bindings;
  CollectVars(**pat, &bindings);
  if (HasFreeVars(**expr, bindings)) {
    std::cerr << "error: expr contains unbound variables: " << stmt
              << std::endl;
    return {};
  }

  // TODO: should Rule just hold Refs?
  return Rule(**pat, **expr, priority);
}

/// parse a program from a list of s-expressions
std::optional<Program> ParseProgram(const std::vector<SExpr> &sexprs) {
  Program program;
  for (const SExpr &sexpr : sexprs) {
    const SList *stmt = std::get_if<SList>(&sexpr);
    if (!stmt) {
      std::cerr << "error: expected statement, got " << sexpr << std::endl;
      return {};
    }

    if (stmt->empty()) {
      std::cerr << "error: empty statement" << std::endl;
      return {};
    }

    // get head
    const SIdent *ident = std::get_if<SIdent>(&stmt->front());
    if (!ident) {
      std::cerr << "error: expected identifier in statement head: " << sexpr
                << std::endl;
      return {};
    }

    if (*ident == "type") {
      // decl-type
      auto type_decl = ParseTypeDecl(*stmt, &program);
      if (!type_decl) {
        std::cerr << "error: failed to parse type declaration" << std::endl;
        return {};
      }
      // TODO: maybe just put this in ParseTypeDecl
      program.type_decls.push_back(type_decl.value());
    } else if (*ident == "decl") {
      auto fn_decl = ParseFnDecl(*stmt, &program);
      if (!fn_decl) {
        std::cerr << "error: failed to parse function declaration" << std::endl;
        return {};
      }
      // TODO
      program.fn_decls.push_back(fn_decl.value());
    } else if (*ident == "constructor") {
      // ctor
      if (!ParseExtern(*stmt, "constructor", &program)) {
        std::cerr << "error: failed to parse extern constructor declaration"
                  << std::endl;
        return {};
      }
    } else if (*ident == "extractor") {
      // xtor
      if (!ParseExtern(*stmt, "extractor", &program)) {
        std::cerr << "error: failed to parse extern extractor declaration"
                  << std::endl;
        return {};
      }
    } else if (*ident == "rule") {
      auto rule = ParseRule(program, *stmt);
      if (!rule) {
        std::cerr << "error: failed to parse rule: " << sexpr << std::endl;
        return {};
      }
      program.fn_rules[std::get<PCall>(rule.value().pattern).fn].push_back(
          rule.value());
    } else {
      std::cerr << "error: unknown statement kind: " << sexpr << std::endl;
      return {};
    }
  }
  return program;
}

} // namespace isle

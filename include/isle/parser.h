#pragma once

#include <iostream>
#include <memory>
#include <optional>
#include <sstream>
#include <unordered_map>
#include <variant>
#include <vector>

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

struct EnumOption {
  std::string name;
  std::vector<std::pair<std::string, std::string>> fields;
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
  std::optional<std::string> ctor;
  std::optional<std::string> xtor;
  std::vector<std::string> arg_types;
  std::string ret_type;
};

struct Program {
  std::unordered_map<Id, std::string> type_names;
  std::unordered_map<std::string, Id> type_ids;
  std::unordered_map<Id, std::string> fn_names;
  std::unordered_map<std::string, Id> fn_ids;

  std::vector<TypeDecl> type_decls;
  std::vector<FnDecl> fn_decls;
};

std::optional<EnumOption> ParseEnumOption(const SExpr &sexpr) {
  const SList *opt = std::get_if<SList>(&sexpr);
  if (!opt) {
    std::cerr << "error: expected enum option definition: " << sexpr
              << std::endl;
    return std::nullopt;
  }

  if (opt->size() < 1 || !std::holds_alternative<SIdent>(opt->front())) {
    std::cerr << "error: expected identifier for enum option name: " << sexpr
              << std::endl;
    return std::nullopt;
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
      return std::nullopt;
    }
    result.fields.emplace_back((*field)[0], (*field)[1]);
  }

  return result;
}

std::optional<Enum> ParseEnum(const SExpr &sexpr) {
  const SList *enum_ = std::get_if<SList>(&sexpr);
  if (!enum_) {
    std::cerr << "error: expected enum definition: " << sexpr << std::endl;
    return std::nullopt;
  }
  if (enum_->size() < 1 || !std::holds_alternative<SIdent>(enum_->front()) ||
      std::get<SIdent>(enum_->front()) != "enum") {
    std::cerr << "error: expected \"enum\" in enum definition: " << sexpr
              << std::endl;
    return std::nullopt;
  }
  // (1) flat list of identifiers
  // (2) list of enum options
  if (enum_->size() < 2) {
    std::cerr << "error: expected at least one variant in enum definition: "
              << sexpr << std::endl;
    return std::nullopt;
  }
  Enum result;
  if (std::holds_alternative<SIdent>((*enum_)[1])) {
    // case (1)
    for (int i = 1; i < enum_->size(); ++i) {
      const SIdent *option_name = std::get_if<SIdent>(&(*enum_)[i]);
      if (!option_name) {
        std::cerr << "error: expected identifier for option name: " << sexpr
                  << std::endl;
        return std::nullopt;
      }
      result.emplace_back(option_name,
                          std::vector<std::pair<std::string, std::string>>{});
    }
  } else {
    // case (2)
    for (int i = 1; i < enum_->size(); ++i) {
      auto opt = ParseEnumOption((*enum_)[i]);
      if (!opt.has_value()) {
        return std::nullopt;
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
    return std::nullopt;
  }
  const SIdent *ident = std::get_if<SIdent>(&stmt[1]);
  if (!ident) {
    std::cerr << "error: expected identifier in type declaration: " << stmt
              << std::endl;
    return std::nullopt;
  }
  // check that the type hasn't already been declared
  if (program->type_ids.count(*ident)) {
    std::cerr << "error: type has already been declared: " << stmt << std::endl;
    return std::nullopt;
  }
  type_decl.name = *ident;

  if (stmt.size() < 3) {
    std::cerr << "error: expected type to be an enum or a primitive: " << stmt
              << std::endl;
    return std::nullopt;
  }
  ident = std::get_if<SIdent>(&stmt[2]);

  if (!ident) {
    // try to parse internal enum
    type_decl.kind = TypeDecl::Kind::Internal;
    type_decl.options = ParseEnum(stmt[2]);
    if (!type_decl.options.has_value()) {
      return std::nullopt;
    }
  } else if (*ident == "external") {
    type_decl.kind = TypeDecl::Kind::External;
    if (stmt.size() != 4) {
      std::cerr << "error: missing enum definition: " << stmt << std::endl;
      return std::nullopt;
    }
    type_decl.options = ParseEnum(stmt[3]);
    if (!type_decl.options.has_value()) {
      return std::nullopt;
    }
  } else if (*ident == "primitive") {
    type_decl.kind = TypeDecl::Kind::Primitive;
    type_decl.options = std::nullopt;
  } else {
    std::cerr << "error: unexpected identifier \"" << *ident << "\": " << stmt
              << std::endl;
    return std::nullopt;
  }

  program->type_names.insert({type_decl.id, type_decl.name});
  program->type_ids.insert({type_decl.name, type_decl.id});

  return type_decl;
}

/// parse a program from a list of s-expressions
std::optional<Program> ParseProgram(const SList &sexprs) {
  Program program;
  for (const SExpr &sexpr : sexprs) {
    const SList *stmt = std::get_if<SList>(&sexpr);
    if (!stmt) {
      std::cerr << "error: expected statement, got " << sexpr << std::endl;
      return std::nullopt;
    }

    if (stmt->empty()) {
      std::cerr << "error: empty statement" << std::endl;
      return std::nullopt;
    }

    // get head
    const SIdent *ident = std::get_if<SIdent>(&stmt->front());
    if (!ident) {
      std::cerr << "error: expected identifier in statement head: " << sexpr
                << std::endl;
      return std::nullopt;
    }

    if (*ident == "type") {
      // decl-type
      auto type_decl = ParseTypeDecl(*stmt, &program);
      if (!type_decl.has_value()) {
        std::cerr << "error: failed to parse type declaration" << std::endl;
        return std::nullopt;
      }
      // TODO: maybe just put this in ParseTypeDecl
      program.type_decls.push_back(type_decl.value());
    } else if (*ident == "decl") {
      // decl-fn
    } else if (*ident == "constructor") {
      // ctor
    } else if (*ident == "extractor") {
      // xtor
    } else if (*ident == "rule") {
      // rule
    } else {
      std::cerr << "error: unknown statement kind: " << sexpr << std::endl;
      return std::nullopt;
    }
  }
}

} // namespace isle

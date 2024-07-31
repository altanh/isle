#pragma once

#include <algorithm>
#include <iostream>
#include <sstream>
#include <string>

#include "ir.h"
#include "visitors.h"

namespace isle {

// TypeDecl -> struct definition, constructors, extractors
void EmitTypeDecl(const Program &program, const TypeDecl &type_decl,
                  std::ostream &os) {
  if (type_decl.kind != TypeDecl::Kind::Internal) {
    // extern enums should already exist in the correct form
    return;
  }

  const auto &opts = type_decl.options.value();

  // for each option, emit a struct
  for (const auto &opt : opts) {
    os << "struct " << type_decl.name << opt.name << " {\n";
    // emit fields
    for (const auto &field : opt.fields) {
      os << "  " << program.type_names.at(field.second) << " " << field.first
         << ";\n";
    }
    os << "\n";
    os << "};\n\n";
  }

  // emit sum type
  os << "using " << type_decl.name << " = std::variant<";
  os << type_decl.name << opts[0].name;
  for (int i = 1; i < opts.size(); ++i) {
    os << ", " << type_decl.name << opts[i].name;
  }
  os << ">;\n\n";

  // emit constructors and extractors
  for (const auto &opt : opts) {
    // TODO(@altanh): support generic context
    os << type_decl.name << " Construct" << type_decl.name << opt.name;
    os << "(";
    if (!opt.fields.empty()) {
      os << "const " << program.type_names.at(opt.fields[0].second) << " &"
         << opt.fields[0].first;
      for (int i = 1; i < opt.fields.size(); ++i) {
        os << ", const " << program.type_names.at(opt.fields[i].second) << " &"
           << opt.fields[i].first;
      }
    }
    os << ") {\n";
    os << "  return " << type_decl.name << opt.name << " {";
    if (!opt.fields.empty()) {
      os << opt.fields[0].first;
      for (int i = 1; i < opt.fields.size(); ++i) {
        os << ", " << opt.fields[i].first;
      }
    }
    os << "};\n";
    os << "}\n\n";

    os << "std::optional<" << type_decl.name << opt.name << "> Extract"
       << type_decl.name << opt.name << "(const " << type_decl.name
       << " &value) {\n";
    os << "  const auto *x = std::get_if<" << type_decl.name << opt.name
       << ">(&value);\n";
    os << "  return x ? *x : {};\n";
    os << "}\n\n";
  }
}

// TODO: expose enum ctors and xtors in isle
// TODO: internal extractor defns
// TODO: indentation helper

struct NameFactory {
  std::string Get(const std::string &name) {
    return name + std::to_string(names_[name]++);
  }

private:
  std::unordered_map<std::string, int> names_;
};

void EmitMatch(const Program &program, const Pattern &pat,
               const std::string &value, std::ostream &os,
               std::unordered_map<std::string, std::string> &bindings,
               NameFactory &nf) {
  return std::visit(
      Visitor{[&](const PCall &call) {
                // look up function, extract, match subterms
                const auto &fn = program.fn_decls[call.fn];
                if (!fn.xtor) {
                  throw std::runtime_error("cannot extract term " + fn.name +
                                           " without extractor!");
                }
                const std::string args_name = nf.Get(fn.name);
                std::ostringstream oss;
                oss << "    std::optional<std::tuple<";
                if (!fn.arg_types.empty()) {
                  oss << program.type_names.at(fn.arg_types[0]);
                  for (int i = 1; i < fn.arg_types.size(); ++i) {
                    oss << ", " << program.type_names.at(fn.arg_types[i]);
                  }
                }
                oss << ">> " << args_name << " = " << fn.xtor.value() << "("
                    << value << ");";
                os << oss.str() << "\n";
                os << "    if (!" << args_name << ") { return {}; }\n";
                for (int i = 0; i < fn.arg_types.size(); ++i) {
                  oss.str("");
                  oss << "std::get<" << i << ">(" << args_name << ".value())";
                  const std::string field =
                      args_name + "_arg" + std::to_string(i);
                  os << "    auto " << field << " = " << oss.str() << ";\n";
                  EmitMatch(program, *call.args[i], field, os, bindings, nf);
                }
              },
              [&](const PBind &bind) {
                // match subterm, then bind var to value
                EmitMatch(program, *bind.pattern, value, os, bindings, nf);
                os << "    auto " << bind.var << " = " << value << ";\n";
                bindings[bind.var] = value;
              },
              [&](const PWildcard &wildcard) {},
              [&](const Var &var) {
                if (bindings.count(var.name)) {
                  // check equality
                  os << "    if (" << var << " != " << value
                     << ") { return {}; }\n";
                } else {
                  os << "    auto " << var << " = " << value << ";\n";
                  bindings[var] = value;
                }
              },
              [&](const IntConst &i) {
                os << "    if (" << i << " != " << value
                   << ") { return {}; }\n";
              }},
      pat);
}

std::string EmitExpr(const Program &program, const Expr &expr, std::ostream &os,
                     NameFactory &nf) {
  return std::visit(
      Visitor{[&](const ECall &call) {
                const auto &fn = program.fn_decls[call.fn];
                // TODO: fix this
                const std::string ctor =
                    fn.ctor.value_or("construct_" + fn.name);
                std::vector<std::string> args;
                for (int i = 0; i < call.args.size(); ++i) {
                  args.emplace_back(EmitExpr(program, *call.args[i], os, nf));
                }
                const std::string name = nf.Get(fn.name);
                for (int i = 0; i < call.args.size(); ++i) {
                  os << "    auto " << name << "_arg" << i << " = " << args[i]
                     << ";\n";
                }
                std::ostringstream oss;
                oss << ctor << "(";
                if (!call.args.empty()) {
                  oss << name << "_arg0";
                  for (int i = 1; i < call.args.size(); ++i) {
                    oss << ", " << name << "_arg" << i;
                  }
                }
                oss << ")";
                return oss.str();
              },
              [&](const Var &var) { return var.name; },
              [&](const IntConst &i) { return std::to_string(i); }},
      expr);
}

void EmitRuleLambda(const Program &program, const Rule &rule,
                    std::ostream &os) {
  // [](arg0, ...) {
  //   // match pattern
  //   ...
  //   // construct expr
  // };
  const PCall &call = std::get<PCall>(rule.pattern);
  const auto &fn = program.fn_decls[call.fn];
  const int nargs = fn.arg_types.size();
  std::unordered_map<std::string, std::string> bindings;
  NameFactory nf;
  os << "[](";
  if (!fn.arg_types.empty()) {
    os << "const " << program.type_names.at(fn.arg_types[0]) << " &arg0";
    for (int i = 1; i < nargs; ++i) {
      os << ", const " << program.type_names.at(fn.arg_types[i]) << " &arg"
         << i;
    }
  }
  os << ") {\n";
  for (int i = 0; i < nargs; ++i) {
    os << "     // match argument " << i << "\n";
    EmitMatch(program, *call.args[i], "arg" + std::to_string(i), os, bindings,
              nf);
  }
  os << "    // construct result expr\n";
  const std::string expr = EmitExpr(program, rule.expr, os, nf);
  os << "    return " << expr << "\n";
  os << "  };\n";
}

void EmitConstructor(const Program &program, Id fn_id, std::ostream &os) {
  const auto &fn = program.fn_decls[fn_id];
  if (fn.external) {
    return;
  }
  if (!program.fn_rules.count(fn_id)) {
    return;
  }
  std::vector<Rule> rules = program.fn_rules.at(fn_id);
  // sort by priority
  std::sort(rules.begin(), rules.end(), [](const Rule &l, const Rule &r) {
    return l.priority > r.priority;
  });
  os << "std::optional<" << program.type_names.at(fn.ret_type) << "> construct_"
     << fn.name << "(";
  if (!fn.arg_types.empty()) {
    os << "const " << program.type_names.at(fn.arg_types[0]) << " &arg0";
    for (int i = 1; i < fn.arg_types.size(); ++i) {
      os << ", const " << program.type_names.at(fn.arg_types[i]) << " &arg"
         << i;
    }
  }
  os << ") {\n";
  for (int i = 0; i < rules.size(); ++i) {
    os << "  auto rule" << i << " = ";
    EmitRuleLambda(program, rules[i], os);
  }
  // return rule0.value_or(rule1.value_or(...ruleN.value()));
  for (int i = 0; i < rules.size(); ++i) {
    if (i == 0) {
      os << "  auto ";
    } else {
      os << "  ";
    }
    os << "result = rule" << i << "(";
    if (!fn.arg_types.empty()) {
      os << "arg0";
      for (int i = 1; i < fn.arg_types.size(); ++i) {
        os << ", arg" << i;
      }
    }
    os << ");\n";
    os << "  if (result) { return result.value() }\n";
  }
  os << "  return {};\n";
  os << "}\n";
}

} // namespace isle
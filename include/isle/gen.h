#pragma once

#include <iostream>

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

} // namespace isle
#include <iostream>
#include <memory>
#include <optional>
#include <string>
#include <tuple>
#include <variant>

using str = std::string;

struct Lam;
struct App;

using E = std::variant<str, Lam, App>;

struct Lam {
  str var;
  std::shared_ptr<E> body;
};

struct App {
  std::shared_ptr<E> func;
  std::shared_ptr<E> arg;
};

// TODO: converting between E and E* is annoying af. Need to come up with a
// better way to do this. Maybe better to return tuple of references?

std::optional<std::tuple<str>> GetVar(const E &e) {
  if (std::holds_alternative<str>(e)) {
    return std::get<str>(e);
  }
  return {};
}

std::optional<std::tuple<str, E>> GetLam(const E &e) {
  if (std::holds_alternative<Lam>(e)) {
    const auto &lam = std::get<Lam>(e);
    return std::make_tuple(lam.var, *lam.body);
  }
  return {};
}

std::optional<std::tuple<E, E>> GetApp(const E &e) {
  if (const App *app = std::get_if<App>(&e)) {
    return std::make_tuple(*app->func, *app->arg);
  }
  return {};
}

E MakeLam(const str &var, const E &body) {
  return Lam{var, std::make_shared<E>(body)};
}

E MakeApp(const E &func, const E &arg) {
  return App{std::make_shared<E>(func), std::make_shared<E>(arg)};
}

template <typename T> T Identity(T &&t) { return t; }
template <typename T> T force(T &&t) { return t; }
template <typename T> T force(std::optional<T> &&t) { return t.value(); }

std::optional<E> construct_subst(const str &arg0, const E &arg1,
                                 const E &arg2) {
  auto rule0 = [](const str &arg0, const E &arg1,
                  const E &arg2) -> std::optional<E> {
    // match argument 0
    auto x = arg0;
    // match argument 1
    auto e1 = arg1;
    // match argument 2
    std::optional<std::tuple<str>> var0 = GetVar(arg2);
    if (!var0) {
      return {};
    }
    auto var0_arg0 = std::get<0>(var0.value());
    if (x != var0_arg0) {
      return {};
    }
    // construct result expr
    auto identity0_arg0 = e1;
    return force(Identity(identity0_arg0));
  };
  auto rule1 = [](const str &arg0, const E &arg1,
                  const E &arg2) -> std::optional<E> {
    // match argument 0
    auto x = arg0;
    // match argument 1
    auto e1 = arg1;
    // match argument 2
    std::optional<std::tuple<str>> var0 = GetVar(arg2);
    if (!var0) {
      return {};
    }
    auto var0_arg0 = std::get<0>(var0.value());
    auto yy = var0_arg0;
    auto y = arg2;
    // construct result expr
    auto identity0_arg0 = y;
    return force(Identity(identity0_arg0));
  };
  auto rule2 = [](const str &arg0, const E &arg1,
                  const E &arg2) -> std::optional<E> {
    // match argument 0
    auto x = arg0;
    // match argument 1
    auto e1 = arg1;
    // match argument 2
    std::optional<std::tuple<str, E>> lam0 = GetLam(arg2);
    if (!lam0) {
      return {};
    }
    auto lam0_arg0 = std::get<0>(lam0.value());
    auto y = lam0_arg0;
    auto lam0_arg1 = std::get<1>(lam0.value());
    auto e2 = lam0_arg1;
    // construct result expr
    auto subst0_arg0 = x;
    auto subst0_arg1 = e1;
    auto subst0_arg2 = e2;
    auto lam1_arg0 = y;
    auto lam1_arg1 =
        force(construct_subst(subst0_arg0, subst0_arg1, subst0_arg2));
    return force(MakeLam(lam1_arg0, lam1_arg1));
  };
  auto rule3 = [](const str &arg0, const E &arg1,
                  const E &arg2) -> std::optional<E> {
    // match argument 0
    auto x = arg0;
    // match argument 1
    auto e1 = arg1;
    // match argument 2
    std::optional<std::tuple<E, E>> app0 = GetApp(arg2);
    if (!app0) {
      return {};
    }
    auto app0_arg0 = std::get<0>(app0.value());
    auto f = app0_arg0;
    auto app0_arg1 = std::get<1>(app0.value());
    auto y = app0_arg1;
    // construct result expr
    auto subst0_arg0 = x;
    auto subst0_arg1 = e1;
    auto subst0_arg2 = f;
    auto subst1_arg0 = x;
    auto subst1_arg1 = e1;
    auto subst1_arg2 = y;
    auto app1_arg0 =
        force(construct_subst(subst0_arg0, subst0_arg1, subst0_arg2));
    auto app1_arg1 =
        force(construct_subst(subst1_arg0, subst1_arg1, subst1_arg2));
    return force(MakeApp(app1_arg0, app1_arg1));
  };
  auto result = rule0(arg0, arg1, arg2);
  if (result) {
    return result.value();
  }
  result = rule1(arg0, arg1, arg2);
  if (result) {
    return result.value();
  }
  result = rule2(arg0, arg1, arg2);
  if (result) {
    return result.value();
  }
  result = rule3(arg0, arg1, arg2);
  if (result) {
    return result.value();
  }
  return {};
}
std::optional<E> construct_eval(const E &arg0) {
  auto rule0 = [](const E &arg0) -> std::optional<E> {
    // match argument 0
    std::optional<std::tuple<E, E>> app0 = GetApp(arg0);
    if (!app0) {
      return {};
    }
    auto app0_arg0 = std::get<0>(app0.value());
    std::optional<std::tuple<str, E>> lam0 = GetLam(app0_arg0);
    if (!lam0) {
      return {};
    }
    auto lam0_arg0 = std::get<0>(lam0.value());
    auto x = lam0_arg0;
    auto lam0_arg1 = std::get<1>(lam0.value());
    auto e = lam0_arg1;
    auto app0_arg1 = std::get<1>(app0.value());
    std::optional<std::tuple<str, E>> lam1 = GetLam(app0_arg1);
    if (!lam1) {
      return {};
    }
    auto lam1_arg0 = std::get<0>(lam1.value());
    auto lam1_arg1 = std::get<1>(lam1.value());
    auto g = app0_arg1;
    // construct result expr
    auto subst0_arg0 = x;
    auto subst0_arg1 = g;
    auto subst0_arg2 = e;
    auto eval0_arg0 =
        force(construct_subst(subst0_arg0, subst0_arg1, subst0_arg2));
    return force(construct_eval(eval0_arg0));
  };
  auto rule1 = [](const E &arg0) -> std::optional<E> {
    // match argument 0
    std::optional<std::tuple<E, E>> app0 = GetApp(arg0);
    if (!app0) {
      return {};
    }
    auto app0_arg0 = std::get<0>(app0.value());
    std::optional<std::tuple<str, E>> lam0 = GetLam(app0_arg0);
    if (!lam0) {
      return {};
    }
    auto lam0_arg0 = std::get<0>(lam0.value());
    auto x = lam0_arg0;
    auto lam0_arg1 = std::get<1>(lam0.value());
    auto e1 = lam0_arg1;
    auto f = app0_arg0;
    auto app0_arg1 = std::get<1>(app0.value());
    auto e2 = app0_arg1;
    // construct result expr
    auto eval0_arg0 = e2;
    auto app1_arg0 = f;
    auto app1_arg1 = force(construct_eval(eval0_arg0));
    auto eval1_arg0 = force(MakeApp(app1_arg0, app1_arg1));
    return force(construct_eval(eval1_arg0));
  };
  auto rule2 = [](const E &arg0) -> std::optional<E> {
    // match argument 0
    std::optional<std::tuple<E, E>> app0 = GetApp(arg0);
    if (!app0) {
      return {};
    }
    auto app0_arg0 = std::get<0>(app0.value());
    auto f = app0_arg0;
    auto app0_arg1 = std::get<1>(app0.value());
    auto x = app0_arg1;
    // construct result expr
    auto eval0_arg0 = f;
    auto app1_arg0 = force(construct_eval(eval0_arg0));
    auto app1_arg1 = x;
    auto eval1_arg0 = force(MakeApp(app1_arg0, app1_arg1));
    return force(construct_eval(eval1_arg0));
  };
  auto rule3 = [](const E &arg0) -> std::optional<E> {
    // match argument 0
    auto e = arg0;
    // construct result expr
    auto identity0_arg0 = e;
    return force(Identity(identity0_arg0));
  };
  auto result = rule0(arg0);
  if (result) {
    return result.value();
  }
  result = rule1(arg0);
  if (result) {
    return result.value();
  }
  result = rule2(arg0);
  if (result) {
    return result.value();
  }
  result = rule3(arg0);
  if (result) {
    return result.value();
  }
  return {};
}

template <class... Ts> struct Visitor : Ts... {
  using Ts::operator()...;
};

template <class... Ts> Visitor(Ts...) -> Visitor<Ts...>;

std::ostream &operator<<(std::ostream &os, const E &e) {
  return std::visit(Visitor{[&](const str &s) -> std::ostream & {
                              os << s;
                              return os;
                            },
                            [&](const Lam &lam) -> std::ostream & {
                              os << "\\" << lam.var << "." << *lam.body;
                              return os;
                            },
                            [&](const App &app) -> std::ostream & {
                              os << "(" << *app.func << " " << *app.arg << ")";
                              return os;
                            }},
                    e);
}

int main() {
  auto fst = std::make_shared<E>(
      Lam{"x", std::make_shared<E>(Lam{"y", std::make_shared<E>("x")})});
  auto snd = std::make_shared<E>(
      Lam{"x", std::make_shared<E>(Lam{"y", std::make_shared<E>("y")})});
  auto id1 = std::make_shared<E>(Lam{"x", std::make_shared<E>("x")});
  auto id2 = std::make_shared<E>(Lam{"y", std::make_shared<E>("y")});
  App app = App{id1, id2};

  auto res = construct_eval(app);
  std::cout << res.value() << std::endl;

  return 0;
}

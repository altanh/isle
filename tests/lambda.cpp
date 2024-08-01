#include <iostream>
#include <memory>
#include <optional>
#include <string>
#include <tuple>
#include <variant>

// using str = std::string;
class str;
struct Lam;
struct App;

using E = std::variant<str, Lam, App>;

class str : public std::string {
  using std::string::string;

public:
  operator std::shared_ptr<E>() const { return std::make_shared<E>(*this); }
};

struct Lam {
  str var;
  std::shared_ptr<E> body;

  operator std::shared_ptr<E>() const { return std::make_shared<E>(*this); }
};

struct App {
  std::shared_ptr<E> func;
  std::shared_ptr<E> arg;

  operator std::shared_ptr<E>() const { return std::make_shared<E>(*this); }
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

#include "lambda_gen.cpp"

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
  // auto fst = std::make_shared<E>(
  //     Lam{"x", std::make_shared<E>(Lam{"y", std::make_shared<E>("x")})});
  // auto snd = std::make_shared<E>(
  //     Lam{"x", std::make_shared<E>(Lam{"y", std::make_shared<E>("y")})});
  // auto id1 = std::make_shared<E>(Lam{"x", std::make_shared<E>("x")});
  // auto id2 = std::make_shared<E>(Lam{"y", std::make_shared<E>("y")});
  auto zero = Lam{"f", Lam{"x", str("x")}};
  // auto succ =
  //     Lam{"n", Lam{"f", Lam{"x", App{App{str("n"), str("f")}, str("x")}}}};
  auto succ =
      Lam{"n", Lam{"f", Lam{"x", App{str("f"),
                                     App{App{str("n"), str("f")}, str("x")}}}}};
  auto id1 = Lam{"x", str("x")};
  auto id2 = Lam{"y", str("y")};
  // App app = App{id2, id1};
  // need alpha conversion?
  App one = App{succ, zero};
  App two = App{succ, one};

  auto res = construct_eval(one);
  std::cout << one << " => " << res.value() << std::endl;

  return 0;
}

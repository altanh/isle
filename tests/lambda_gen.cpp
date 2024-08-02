std::optional<unit> construct_bound(const str &arg0, const E &arg1) {
  // (bound x (lam x e)) => (unit)
  auto rule0 = [](const str &arg0, const E &arg1) -> std::optional<unit> {
    // match argument 0
    auto x = arg0;
    // match argument 1
    std::optional<std::tuple<str, E>> lam0 = GetLam(arg1);
    if (!lam0) { return {}; }
    auto lam0_arg0 = std::get<0>(*lam0);
    if (x != lam0_arg0) { return {}; }
    auto lam0_arg1 = std::get<1>(*lam0);
    auto e = lam0_arg1;
    // construct result expr
    return force(std::make_tuple());
  };
  // (bound x (lam y e)) => (bound x e)
  auto rule1 = [](const str &arg0, const E &arg1) -> std::optional<unit> {
    // match argument 0
    auto x = arg0;
    // match argument 1
    std::optional<std::tuple<str, E>> lam0 = GetLam(arg1);
    if (!lam0) { return {}; }
    auto lam0_arg0 = std::get<0>(*lam0);
    auto y = lam0_arg0;
    auto lam0_arg1 = std::get<1>(*lam0);
    auto e = lam0_arg1;
    // (if-let _ (strneq x y))
    auto strneq0_arg0 = x;
    auto strneq0_arg1 = y;
    auto maybe0 = StrNeq(strneq0_arg0, strneq0_arg1);
    if (!maybe0) { return {}; }
    auto value0 = *maybe0;
    // construct result expr
    auto bound0_arg0 = x;
    auto bound0_arg1 = e;
    return force(construct_bound(bound0_arg0, bound0_arg1));
  };
  // (bound x (app f y)) => (unit)
  auto rule2 = [](const str &arg0, const E &arg1) -> std::optional<unit> {
    // match argument 0
    auto x = arg0;
    // match argument 1
    std::optional<std::tuple<E, E>> app0 = GetApp(arg1);
    if (!app0) { return {}; }
    auto app0_arg0 = std::get<0>(*app0);
    auto f = app0_arg0;
    auto app0_arg1 = std::get<1>(*app0);
    auto y = app0_arg1;
    // (if-let _ (bound x f))
    auto bound0_arg0 = x;
    auto bound0_arg1 = f;
    auto maybe0 = construct_bound(bound0_arg0, bound0_arg1);
    if (!maybe0) { return {}; }
    auto value0 = *maybe0;
    // (if-let _ (bound x y))
    auto bound1_arg0 = x;
    auto bound1_arg1 = y;
    auto maybe1 = construct_bound(bound1_arg0, bound1_arg1);
    if (!maybe1) { return {}; }
    auto value1 = *maybe1;
    // construct result expr
    return force(std::make_tuple());
  };
  // (bound x (var y)) => (unit)
  auto rule3 = [](const str &arg0, const E &arg1) -> std::optional<unit> {
    // match argument 0
    auto x = arg0;
    // match argument 1
    std::optional<std::tuple<str>> var0 = GetVar(arg1);
    if (!var0) { return {}; }
    auto var0_arg0 = std::get<0>(*var0);
    auto y = var0_arg0;
    // (if-let _ (strneq x y))
    auto strneq0_arg0 = x;
    auto strneq0_arg1 = y;
    auto maybe0 = StrNeq(strneq0_arg0, strneq0_arg1);
    if (!maybe0) { return {}; }
    auto value0 = *maybe0;
    // construct result expr
    return force(std::make_tuple());
  };
  auto result = rule0(arg0, arg1);
  if (result) { return result.value(); }
  result = rule1(arg0, arg1);
  if (result) { return result.value(); }
  result = rule2(arg0, arg1);
  if (result) { return result.value(); }
  result = rule3(arg0, arg1);
  if (result) { return result.value(); }
  return {};
}
std::optional<str> construct_fresh(const str &arg0, const E &arg1) {
  // (fresh x e) => (identity x)
  auto rule0 = [](const str &arg0, const E &arg1) -> std::optional<str> {
    // match argument 0
    auto x = arg0;
    // match argument 1
    auto e = arg1;
    // (if-let _ (bound x e))
    auto bound0_arg0 = x;
    auto bound0_arg1 = e;
    auto maybe0 = construct_bound(bound0_arg0, bound0_arg1);
    if (!maybe0) { return {}; }
    auto value0 = *maybe0;
    // construct result expr
    auto identity0_arg0 = x;
    return force(Identity(identity0_arg0));
  };
  // (fresh x e) => (fresh (prime x) e)
  auto rule1 = [](const str &arg0, const E &arg1) -> std::optional<str> {
    // match argument 0
    auto x = arg0;
    // match argument 1
    auto e = arg1;
    // construct result expr
    auto prime0_arg0 = x;
    auto fresh0_arg0 = force(Prime(prime0_arg0));
    auto fresh0_arg1 = e;
    return force(construct_fresh(fresh0_arg0, fresh0_arg1));
  };
  auto result = rule0(arg0, arg1);
  if (result) { return result.value(); }
  result = rule1(arg0, arg1);
  if (result) { return result.value(); }
  return {};
}
std::optional<E> construct_alpha(const str &arg0, const str &arg1, const E &arg2) {
  // (alpha x x e) => (identity e)
  auto rule0 = [](const str &arg0, const str &arg1, const E &arg2) -> std::optional<E> {
    // match argument 0
    auto x = arg0;
    // match argument 1
    if (x != arg1) { return {}; }
    // match argument 2
    auto e = arg2;
    // construct result expr
    auto identity0_arg0 = e;
    return force(Identity(identity0_arg0));
  };
  // (alpha x xx (@ f (lam x e))) => (identity f)
  auto rule1 = [](const str &arg0, const str &arg1, const E &arg2) -> std::optional<E> {
    // match argument 0
    auto x = arg0;
    // match argument 1
    auto xx = arg1;
    // match argument 2
    std::optional<std::tuple<str, E>> lam0 = GetLam(arg2);
    if (!lam0) { return {}; }
    auto lam0_arg0 = std::get<0>(*lam0);
    if (x != lam0_arg0) { return {}; }
    auto lam0_arg1 = std::get<1>(*lam0);
    auto e = lam0_arg1;
    auto f = arg2;
    // construct result expr
    auto identity0_arg0 = f;
    return force(Identity(identity0_arg0));
  };
  // (alpha x xx (var x)) => (var xx)
  auto rule2 = [](const str &arg0, const str &arg1, const E &arg2) -> std::optional<E> {
    // match argument 0
    auto x = arg0;
    // match argument 1
    auto xx = arg1;
    // match argument 2
    std::optional<std::tuple<str>> var0 = GetVar(arg2);
    if (!var0) { return {}; }
    auto var0_arg0 = std::get<0>(*var0);
    if (x != var0_arg0) { return {}; }
    // construct result expr
    auto var1_arg0 = xx;
    return force(MakeVar(var1_arg0));
  };
  // (alpha x xx (lam y e)) => (lam y (alpha x xx e))
  auto rule3 = [](const str &arg0, const str &arg1, const E &arg2) -> std::optional<E> {
    // match argument 0
    auto x = arg0;
    // match argument 1
    auto xx = arg1;
    // match argument 2
    std::optional<std::tuple<str, E>> lam0 = GetLam(arg2);
    if (!lam0) { return {}; }
    auto lam0_arg0 = std::get<0>(*lam0);
    auto y = lam0_arg0;
    auto lam0_arg1 = std::get<1>(*lam0);
    auto e = lam0_arg1;
    // construct result expr
    auto alpha0_arg0 = x;
    auto alpha0_arg1 = xx;
    auto alpha0_arg2 = e;
    auto lam1_arg0 = y;
    auto lam1_arg1 = force(construct_alpha(alpha0_arg0, alpha0_arg1, alpha0_arg2));
    return force(MakeLam(lam1_arg0, lam1_arg1));
  };
  // (alpha x xx (@ y (var yy))) => (identity y)
  auto rule4 = [](const str &arg0, const str &arg1, const E &arg2) -> std::optional<E> {
    // match argument 0
    auto x = arg0;
    // match argument 1
    auto xx = arg1;
    // match argument 2
    std::optional<std::tuple<str>> var0 = GetVar(arg2);
    if (!var0) { return {}; }
    auto var0_arg0 = std::get<0>(*var0);
    auto yy = var0_arg0;
    auto y = arg2;
    // construct result expr
    auto identity0_arg0 = y;
    return force(Identity(identity0_arg0));
  };
  // (alpha x xx (app M N)) => (app (alpha x xx M) (alpha x xx N))
  auto rule5 = [](const str &arg0, const str &arg1, const E &arg2) -> std::optional<E> {
    // match argument 0
    auto x = arg0;
    // match argument 1
    auto xx = arg1;
    // match argument 2
    std::optional<std::tuple<E, E>> app0 = GetApp(arg2);
    if (!app0) { return {}; }
    auto app0_arg0 = std::get<0>(*app0);
    auto M = app0_arg0;
    auto app0_arg1 = std::get<1>(*app0);
    auto N = app0_arg1;
    // construct result expr
    auto alpha0_arg0 = x;
    auto alpha0_arg1 = xx;
    auto alpha0_arg2 = M;
    auto alpha1_arg0 = x;
    auto alpha1_arg1 = xx;
    auto alpha1_arg2 = N;
    auto app1_arg0 = force(construct_alpha(alpha0_arg0, alpha0_arg1, alpha0_arg2));
    auto app1_arg1 = force(construct_alpha(alpha1_arg0, alpha1_arg1, alpha1_arg2));
    return force(MakeApp(app1_arg0, app1_arg1));
  };
  auto result = rule0(arg0, arg1, arg2);
  if (result) { return result.value(); }
  result = rule1(arg0, arg1, arg2);
  if (result) { return result.value(); }
  result = rule2(arg0, arg1, arg2);
  if (result) { return result.value(); }
  result = rule3(arg0, arg1, arg2);
  if (result) { return result.value(); }
  result = rule4(arg0, arg1, arg2);
  if (result) { return result.value(); }
  result = rule5(arg0, arg1, arg2);
  if (result) { return result.value(); }
  return {};
}
std::optional<E> construct_subst(const str &arg0, const E &arg1, const E &arg2) {
  // (subst x e1 (var x)) => (identity e1)
  auto rule0 = [](const str &arg0, const E &arg1, const E &arg2) -> std::optional<E> {
    // match argument 0
    auto x = arg0;
    // match argument 1
    auto e1 = arg1;
    // match argument 2
    std::optional<std::tuple<str>> var0 = GetVar(arg2);
    if (!var0) { return {}; }
    auto var0_arg0 = std::get<0>(*var0);
    if (x != var0_arg0) { return {}; }
    // construct result expr
    auto identity0_arg0 = e1;
    return force(Identity(identity0_arg0));
  };
  // (subst x e1 (@ y (var yy))) => (identity y)
  auto rule1 = [](const str &arg0, const E &arg1, const E &arg2) -> std::optional<E> {
    // match argument 0
    auto x = arg0;
    // match argument 1
    auto e1 = arg1;
    // match argument 2
    std::optional<std::tuple<str>> var0 = GetVar(arg2);
    if (!var0) { return {}; }
    auto var0_arg0 = std::get<0>(*var0);
    auto yy = var0_arg0;
    auto y = arg2;
    // construct result expr
    auto identity0_arg0 = y;
    return force(Identity(identity0_arg0));
  };
  // (subst x e1 (@ f (lam x e2))) => (identity f)
  auto rule2 = [](const str &arg0, const E &arg1, const E &arg2) -> std::optional<E> {
    // match argument 0
    auto x = arg0;
    // match argument 1
    auto e1 = arg1;
    // match argument 2
    std::optional<std::tuple<str, E>> lam0 = GetLam(arg2);
    if (!lam0) { return {}; }
    auto lam0_arg0 = std::get<0>(*lam0);
    if (x != lam0_arg0) { return {}; }
    auto lam0_arg1 = std::get<1>(*lam0);
    auto e2 = lam0_arg1;
    auto f = arg2;
    // construct result expr
    auto identity0_arg0 = f;
    return force(Identity(identity0_arg0));
  };
  // (subst x e1 (@ f (lam y e2))) => (let ((yy (fresh y e1))) (lam yy (subst x e1 (alpha y yy e2))))
  auto rule3 = [](const str &arg0, const E &arg1, const E &arg2) -> std::optional<E> {
    // match argument 0
    auto x = arg0;
    // match argument 1
    auto e1 = arg1;
    // match argument 2
    std::optional<std::tuple<str, E>> lam0 = GetLam(arg2);
    if (!lam0) { return {}; }
    auto lam0_arg0 = std::get<0>(*lam0);
    auto y = lam0_arg0;
    auto lam0_arg1 = std::get<1>(*lam0);
    auto e2 = lam0_arg1;
    auto f = arg2;
    // construct result expr
    auto fresh0_arg0 = y;
    auto fresh0_arg1 = e1;
    auto yy = force(construct_fresh(fresh0_arg0, fresh0_arg1));
    auto alpha0_arg0 = y;
    auto alpha0_arg1 = yy;
    auto alpha0_arg2 = e2;
    auto subst0_arg0 = x;
    auto subst0_arg1 = e1;
    auto subst0_arg2 = force(construct_alpha(alpha0_arg0, alpha0_arg1, alpha0_arg2));
    auto lam1_arg0 = yy;
    auto lam1_arg1 = force(construct_subst(subst0_arg0, subst0_arg1, subst0_arg2));
    return force(MakeLam(lam1_arg0, lam1_arg1));
  };
  // (subst x e1 (app f y)) => (app (subst x e1 f) (subst x e1 y))
  auto rule4 = [](const str &arg0, const E &arg1, const E &arg2) -> std::optional<E> {
    // match argument 0
    auto x = arg0;
    // match argument 1
    auto e1 = arg1;
    // match argument 2
    std::optional<std::tuple<E, E>> app0 = GetApp(arg2);
    if (!app0) { return {}; }
    auto app0_arg0 = std::get<0>(*app0);
    auto f = app0_arg0;
    auto app0_arg1 = std::get<1>(*app0);
    auto y = app0_arg1;
    // construct result expr
    auto subst0_arg0 = x;
    auto subst0_arg1 = e1;
    auto subst0_arg2 = f;
    auto subst1_arg0 = x;
    auto subst1_arg1 = e1;
    auto subst1_arg2 = y;
    auto app1_arg0 = force(construct_subst(subst0_arg0, subst0_arg1, subst0_arg2));
    auto app1_arg1 = force(construct_subst(subst1_arg0, subst1_arg1, subst1_arg2));
    return force(MakeApp(app1_arg0, app1_arg1));
  };
  auto result = rule0(arg0, arg1, arg2);
  if (result) { return result.value(); }
  result = rule1(arg0, arg1, arg2);
  if (result) { return result.value(); }
  result = rule2(arg0, arg1, arg2);
  if (result) { return result.value(); }
  result = rule3(arg0, arg1, arg2);
  if (result) { return result.value(); }
  result = rule4(arg0, arg1, arg2);
  if (result) { return result.value(); }
  return {};
}
std::optional<E> construct_eval_cbv(const E &arg0) {
  // (eval_cbv (app (lam x e) (@ g (lam _ _)))) => (eval_cbv (subst x g e))
  auto rule0 = [](const E &arg0) -> std::optional<E> {
    // match argument 0
    std::optional<std::tuple<E, E>> app0 = GetApp(arg0);
    if (!app0) { return {}; }
    auto app0_arg0 = std::get<0>(*app0);
    std::optional<std::tuple<str, E>> lam0 = GetLam(app0_arg0);
    if (!lam0) { return {}; }
    auto lam0_arg0 = std::get<0>(*lam0);
    auto x = lam0_arg0;
    auto lam0_arg1 = std::get<1>(*lam0);
    auto e = lam0_arg1;
    auto app0_arg1 = std::get<1>(*app0);
    std::optional<std::tuple<str, E>> lam1 = GetLam(app0_arg1);
    if (!lam1) { return {}; }
    auto lam1_arg0 = std::get<0>(*lam1);
    auto lam1_arg1 = std::get<1>(*lam1);
    auto g = app0_arg1;
    // construct result expr
    auto subst0_arg0 = x;
    auto subst0_arg1 = g;
    auto subst0_arg2 = e;
    auto eval_cbv0_arg0 = force(construct_subst(subst0_arg0, subst0_arg1, subst0_arg2));
    return force(construct_eval_cbv(eval_cbv0_arg0));
  };
  // (eval_cbv (app (@ f (lam x e1)) e2)) => (eval_cbv (app f (eval_cbv e2)))
  auto rule1 = [](const E &arg0) -> std::optional<E> {
    // match argument 0
    std::optional<std::tuple<E, E>> app0 = GetApp(arg0);
    if (!app0) { return {}; }
    auto app0_arg0 = std::get<0>(*app0);
    std::optional<std::tuple<str, E>> lam0 = GetLam(app0_arg0);
    if (!lam0) { return {}; }
    auto lam0_arg0 = std::get<0>(*lam0);
    auto x = lam0_arg0;
    auto lam0_arg1 = std::get<1>(*lam0);
    auto e1 = lam0_arg1;
    auto f = app0_arg0;
    auto app0_arg1 = std::get<1>(*app0);
    auto e2 = app0_arg1;
    // construct result expr
    auto eval_cbv0_arg0 = e2;
    auto app1_arg0 = f;
    auto app1_arg1 = force(construct_eval_cbv(eval_cbv0_arg0));
    auto eval_cbv1_arg0 = force(MakeApp(app1_arg0, app1_arg1));
    return force(construct_eval_cbv(eval_cbv1_arg0));
  };
  // (eval_cbv (app f x)) => (eval_cbv (app (eval_cbv f) x))
  auto rule2 = [](const E &arg0) -> std::optional<E> {
    // match argument 0
    std::optional<std::tuple<E, E>> app0 = GetApp(arg0);
    if (!app0) { return {}; }
    auto app0_arg0 = std::get<0>(*app0);
    auto f = app0_arg0;
    auto app0_arg1 = std::get<1>(*app0);
    auto x = app0_arg1;
    // construct result expr
    auto eval_cbv0_arg0 = f;
    auto app1_arg0 = force(construct_eval_cbv(eval_cbv0_arg0));
    auto app1_arg1 = x;
    auto eval_cbv1_arg0 = force(MakeApp(app1_arg0, app1_arg1));
    return force(construct_eval_cbv(eval_cbv1_arg0));
  };
  // (eval_cbv e) => (identity e)
  auto rule3 = [](const E &arg0) -> std::optional<E> {
    // match argument 0
    auto e = arg0;
    // construct result expr
    auto identity0_arg0 = e;
    return force(Identity(identity0_arg0));
  };
  auto result = rule0(arg0);
  if (result) { return result.value(); }
  result = rule1(arg0);
  if (result) { return result.value(); }
  result = rule2(arg0);
  if (result) { return result.value(); }
  result = rule3(arg0);
  if (result) { return result.value(); }
  return {};
}
std::optional<E> construct_evaln(const E &arg0) {
  // (evaln (app (lam x e) N)) => (evaln (subst x N e))
  auto rule0 = [](const E &arg0) -> std::optional<E> {
    // match argument 0
    std::optional<std::tuple<E, E>> app0 = GetApp(arg0);
    if (!app0) { return {}; }
    auto app0_arg0 = std::get<0>(*app0);
    std::optional<std::tuple<str, E>> lam0 = GetLam(app0_arg0);
    if (!lam0) { return {}; }
    auto lam0_arg0 = std::get<0>(*lam0);
    auto x = lam0_arg0;
    auto lam0_arg1 = std::get<1>(*lam0);
    auto e = lam0_arg1;
    auto app0_arg1 = std::get<1>(*app0);
    auto N = app0_arg1;
    // construct result expr
    auto subst0_arg0 = x;
    auto subst0_arg1 = N;
    auto subst0_arg2 = e;
    auto evaln0_arg0 = force(construct_subst(subst0_arg0, subst0_arg1, subst0_arg2));
    return force(construct_evaln(evaln0_arg0));
  };
  // (evaln (app (@ x (var xx)) N)) => (app x (evaln N))
  auto rule1 = [](const E &arg0) -> std::optional<E> {
    // match argument 0
    std::optional<std::tuple<E, E>> app0 = GetApp(arg0);
    if (!app0) { return {}; }
    auto app0_arg0 = std::get<0>(*app0);
    std::optional<std::tuple<str>> var0 = GetVar(app0_arg0);
    if (!var0) { return {}; }
    auto var0_arg0 = std::get<0>(*var0);
    auto xx = var0_arg0;
    auto x = app0_arg0;
    auto app0_arg1 = std::get<1>(*app0);
    auto N = app0_arg1;
    // construct result expr
    auto evaln0_arg0 = N;
    auto app1_arg0 = x;
    auto app1_arg1 = force(construct_evaln(evaln0_arg0));
    return force(MakeApp(app1_arg0, app1_arg1));
  };
  // (evaln (app M N)) => (evaln (app (evaln M) N))
  auto rule2 = [](const E &arg0) -> std::optional<E> {
    // match argument 0
    std::optional<std::tuple<E, E>> app0 = GetApp(arg0);
    if (!app0) { return {}; }
    auto app0_arg0 = std::get<0>(*app0);
    auto M = app0_arg0;
    auto app0_arg1 = std::get<1>(*app0);
    auto N = app0_arg1;
    // construct result expr
    auto evaln0_arg0 = M;
    auto app1_arg0 = force(construct_evaln(evaln0_arg0));
    auto app1_arg1 = N;
    auto evaln1_arg0 = force(MakeApp(app1_arg0, app1_arg1));
    return force(construct_evaln(evaln1_arg0));
  };
  // (evaln (lam x e)) => (lam x (evaln e))
  auto rule3 = [](const E &arg0) -> std::optional<E> {
    // match argument 0
    std::optional<std::tuple<str, E>> lam0 = GetLam(arg0);
    if (!lam0) { return {}; }
    auto lam0_arg0 = std::get<0>(*lam0);
    auto x = lam0_arg0;
    auto lam0_arg1 = std::get<1>(*lam0);
    auto e = lam0_arg1;
    // construct result expr
    auto evaln0_arg0 = e;
    auto lam1_arg0 = x;
    auto lam1_arg1 = force(construct_evaln(evaln0_arg0));
    return force(MakeLam(lam1_arg0, lam1_arg1));
  };
  // (evaln (@ x (var xx))) => (identity x)
  auto rule4 = [](const E &arg0) -> std::optional<E> {
    // match argument 0
    std::optional<std::tuple<str>> var0 = GetVar(arg0);
    if (!var0) { return {}; }
    auto var0_arg0 = std::get<0>(*var0);
    auto xx = var0_arg0;
    auto x = arg0;
    // construct result expr
    auto identity0_arg0 = x;
    return force(Identity(identity0_arg0));
  };
  auto result = rule0(arg0);
  if (result) { return result.value(); }
  result = rule1(arg0);
  if (result) { return result.value(); }
  result = rule2(arg0);
  if (result) { return result.value(); }
  result = rule3(arg0);
  if (result) { return result.value(); }
  result = rule4(arg0);
  if (result) { return result.value(); }
  return {};
}
std::optional<E> construct_eval(const E &arg0) {
  // (eval e) => (evaln e)
  auto rule0 = [](const E &arg0) -> std::optional<E> {
    // match argument 0
    auto e = arg0;
    // construct result expr
    auto evaln0_arg0 = e;
    return force(construct_evaln(evaln0_arg0));
  };
  auto result = rule0(arg0);
  if (result) { return result.value(); }
  return {};
}

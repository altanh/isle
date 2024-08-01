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
    auto var0_arg0 = std::get<0>(var0.value());
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
    auto var0_arg0 = std::get<0>(var0.value());
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
    auto lam0_arg0 = std::get<0>(lam0.value());
    if (x != lam0_arg0) { return {}; }
    auto lam0_arg1 = std::get<1>(lam0.value());
    auto e2 = lam0_arg1;
    auto f = arg2;
    // construct result expr
    auto identity0_arg0 = f;
    return force(Identity(identity0_arg0));
  };
  // (subst x e1 (lam y e2)) => (lam y (subst x e1 e2))
  auto rule3 = [](const str &arg0, const E &arg1, const E &arg2) -> std::optional<E> {
     // match argument 0
    auto x = arg0;
     // match argument 1
    auto e1 = arg1;
     // match argument 2
    std::optional<std::tuple<str, E>> lam0 = GetLam(arg2);
    if (!lam0) { return {}; }
    auto lam0_arg0 = std::get<0>(lam0.value());
    auto y = lam0_arg0;
    auto lam0_arg1 = std::get<1>(lam0.value());
    auto e2 = lam0_arg1;
    // construct result expr
    auto subst0_arg0 = x;
    auto subst0_arg1 = e1;
    auto subst0_arg2 = e2;
    auto lam1_arg0 = y;
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
    auto app0_arg0 = std::get<0>(app0.value());
    std::optional<std::tuple<str, E>> lam0 = GetLam(app0_arg0);
    if (!lam0) { return {}; }
    auto lam0_arg0 = std::get<0>(lam0.value());
    auto x = lam0_arg0;
    auto lam0_arg1 = std::get<1>(lam0.value());
    auto e = lam0_arg1;
    auto app0_arg1 = std::get<1>(app0.value());
    std::optional<std::tuple<str, E>> lam1 = GetLam(app0_arg1);
    if (!lam1) { return {}; }
    auto lam1_arg0 = std::get<0>(lam1.value());
    auto lam1_arg1 = std::get<1>(lam1.value());
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
    auto app0_arg0 = std::get<0>(app0.value());
    std::optional<std::tuple<str, E>> lam0 = GetLam(app0_arg0);
    if (!lam0) { return {}; }
    auto lam0_arg0 = std::get<0>(lam0.value());
    auto x = lam0_arg0;
    auto lam0_arg1 = std::get<1>(lam0.value());
    auto e1 = lam0_arg1;
    auto f = app0_arg0;
    auto app0_arg1 = std::get<1>(app0.value());
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
    auto app0_arg0 = std::get<0>(app0.value());
    auto f = app0_arg0;
    auto app0_arg1 = std::get<1>(app0.value());
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
std::optional<E> construct_eval_normal(const E &arg0) {
  // (eval_normal (app (lam x e) y)) => (eval_normal (subst x y e))
  auto rule0 = [](const E &arg0) -> std::optional<E> {
     // match argument 0
    std::optional<std::tuple<E, E>> app0 = GetApp(arg0);
    if (!app0) { return {}; }
    auto app0_arg0 = std::get<0>(app0.value());
    std::optional<std::tuple<str, E>> lam0 = GetLam(app0_arg0);
    if (!lam0) { return {}; }
    auto lam0_arg0 = std::get<0>(lam0.value());
    auto x = lam0_arg0;
    auto lam0_arg1 = std::get<1>(lam0.value());
    auto e = lam0_arg1;
    auto app0_arg1 = std::get<1>(app0.value());
    auto y = app0_arg1;
    // construct result expr
    auto subst0_arg0 = x;
    auto subst0_arg1 = y;
    auto subst0_arg2 = e;
    auto eval_normal0_arg0 = force(construct_subst(subst0_arg0, subst0_arg1, subst0_arg2));
    return force(construct_eval_normal(eval_normal0_arg0));
  };
  // (eval_normal (lam x e)) => (lam x (eval_normal e))
  auto rule1 = [](const E &arg0) -> std::optional<E> {
     // match argument 0
    std::optional<std::tuple<str, E>> lam0 = GetLam(arg0);
    if (!lam0) { return {}; }
    auto lam0_arg0 = std::get<0>(lam0.value());
    auto x = lam0_arg0;
    auto lam0_arg1 = std::get<1>(lam0.value());
    auto e = lam0_arg1;
    // construct result expr
    auto eval_normal0_arg0 = e;
    auto lam1_arg0 = x;
    auto lam1_arg1 = force(construct_eval_normal(eval_normal0_arg0));
    return force(MakeLam(lam1_arg0, lam1_arg1));
  };
  // (eval_normal e) => (identity e)
  auto rule2 = [](const E &arg0) -> std::optional<E> {
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
  return {};
}
std::optional<E> construct_eval(const E &arg0) {
  // (eval e) => (eval_cbv e)
  auto rule0 = [](const E &arg0) -> std::optional<E> {
     // match argument 0
    auto e = arg0;
    // construct result expr
    auto eval_cbv0_arg0 = e;
    return force(construct_eval_cbv(eval_cbv0_arg0));
  };
  auto result = rule0(arg0);
  if (result) { return result.value(); }
  return {};
}

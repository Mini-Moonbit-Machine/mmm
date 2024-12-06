typealias Name = @knf.Name

typealias Type = @knf.Type

typealias Knf = @knf.Knf

pub struct JsLoweringCtx {
  knf : Knf
  mut counter : Int
  tracer : @util.SubTracer
}

pub fn JsLoweringCtx::new(knf : Knf, tracer : @util.SubTracer) -> JsLoweringCtx {
  { knf, tracer, counter: 0 }
}

pub fn JsLoweringCtx::lower(self : JsLoweringCtx) -> JsCore {
  let entry = JsFn::{
    name: Some("thunk_main"),
    params: ["k"],
    expr: self.lower_expr(self.knf, default_kont),
  }
  { entry, }
}

fn JsLoweringCtx::to_js_name(self : JsLoweringCtx, name : Name) -> String {
  let _ = self
  name.to_string().replace_all(old=".", new="_").replace_all(old="$", new="_")
}

fn JsLoweringCtx::to_js_var(self : JsLoweringCtx, name : Name) -> JsExpr {
  Var(self.to_js_name(name))
}

fn JsLoweringCtx::lower_fn(self : JsLoweringCtx, fn_def : @knf.FuncDef) -> JsFn {
  let { name, ty: _, args, body } = fn_def
  let params = args.map(fn(n) { self.to_js_name(n.0) })
  params.push("k")
  JsFn::{
    name: Some(self.to_js_name(name)),
    params,
    expr: self.lower_expr(body, default_kont),
  }
}

fn thunk(expr : JsExpr) -> JsExpr {
  Thunk(expr)
}

fn default_kont(expr : JsExpr) -> JsExpr {
  Call("k", [expr]) |> thunk
}

fn external(f : String, args : Array[JsExpr]) -> JsExpr {
  Call("$external", [Var(f)] + args)
}

fn id_kont(expr : JsExpr) -> JsExpr {
  expr
}

fn JsLoweringCtx::lower_expr(
  self : JsLoweringCtx,
  knf : Knf,
  kont : (JsExpr) -> JsExpr
) -> JsExpr {
  fn get_op(knf : Knf) -> String {
    match knf {
      IfEq(_) => "==="
      IfLe(_) => "<="
      _ => self.tracer.abort("unexpected knf")
    }
  }

  match knf {
    Knf::Unit => Lit("null") |> kont
    Bool(b) => Lit(b.to_string()) |> kont
    Int(i) => Lit(i.to_string()) |> kont
    Double(d) => Lit(d.to_string()) |> kont
    String(s) => Lit(s.escape()) |> kont
    Neg(n) => UnOp("-", self.to_js_name(n)) |> kont
    Add(l, r) => BinOp("+", self.to_js_name(l), self.to_js_name(r)) |> kont
    Sub(l, r) => BinOp("-", self.to_js_name(l), self.to_js_name(r)) |> kont
    Mul(l, r) => BinOp("*", self.to_js_name(l), self.to_js_name(r)) |> kont
    Div(l, r) => IntBinOp("/", self.to_js_name(l), self.to_js_name(r)) |> kont
    FNeg(n) => UnOp("-", self.to_js_name(n)) |> kont
    FAdd(l, r) => BinOp("+", self.to_js_name(l), self.to_js_name(r)) |> kont
    FSub(l, r) => BinOp("-", self.to_js_name(l), self.to_js_name(r)) |> kont
    FMul(l, r) => BinOp("*", self.to_js_name(l), self.to_js_name(r)) |> kont
    FDiv(l, r) => BinOp("/", self.to_js_name(l), self.to_js_name(r)) |> kont
    IfEq(l, r, t, f) | IfLe(l, r, t, f) =>
      If(
        BinOp(get_op(knf), self.to_js_name(l), self.to_js_name(r)),
        self.lower_expr(t, kont),
        self.lower_expr(f, kont),
      )
    Var(n) => self.to_js_var(n) |> kont
    LetRec(f, b) => LetRec(self.lower_fn(f), self.lower_expr(b, kont))
    Let((n, _), IfEq(l, r, t, f) | IfLe(l, r, t, f) as cond, b) => {
      let b = self.lower_expr(b, fn(x) { x |> kont })
      let jp = "j" + self.counter.to_string()
      let k = JsFn::{ name: Some(jp), params: [self.to_js_name(n)], expr: b }
      self.counter += 1
      let kont = fn(x) { Call(jp, [x]) |> thunk }
      LetFn(
        k,
        If(
          BinOp(get_op(cond), self.to_js_name(l), self.to_js_name(r)),
          self.lower_expr(t, kont),
          self.lower_expr(f, kont),
        ),
      )
    }
    Let((n, _), Apply(f, args), b) => {
      let f = self.to_js_name(f)
      let args = args.map(fn(n) { self.to_js_var(n) })
      let b = self.lower_expr(b, fn(x) { x |> kont })
      let k = Fn(JsFn::{ name: None, params: [self.to_js_name(n)], expr: b })
      args.push(k)
      Call(f, args) |> thunk
    }
    Let((n, _), ExternalFunctionApplication(f, args), b) => {
      let args = args.map(fn(n) { self.to_js_var(n) })
      let b = self.lower_expr(b, fn(x) { x |> kont })
      let k = Fn(JsFn::{ name: None, params: [self.to_js_name(n)], expr: b })
      external(f, [k] + args) |> thunk
    }
    Let((n, _), v, b) =>
      Let(
        self.to_js_name(n),
        self.lower_expr(v, id_kont),
        self.lower_expr(b, kont),
      )
    LetTuple(xs, y, b) | LetUnboxedTuple(xs, y, b) =>
      LetTuple(
        xs.map(fn(n) { self.to_js_name(n.0) }),
        self.to_js_var(y),
        self.lower_expr(b, kont),
      )
    Get(a, i) => Get(self.to_js_name(a), self.to_js_name(i)) |> kont
    Put(a, i, v) =>
      Put(self.to_js_name(a), self.to_js_name(i), self.to_js_name(v)) |> kont
    Apply(f, args) => {
      let f = self.to_js_name(f)
      let args = args.map(fn(n) { self.to_js_var(n) })
      args.push(Fn(JsFn::{ name: None, params: ["x"], expr: kont(Var("x")) }))
      Call(f, args) |> thunk
    }
    Tuple(elems) | UnboxedTuple(elems) =>
      Tuple(elems.map(fn(n) { self.to_js_name(n) })) |> kont
    ExternalArray(_) => self.tracer.abort("ExternalArray not supported")
    ExternalFunctionApplication(f, args) => {
      let args_k = [
        Fn(JsFn::{ name: None, params: ["x"], expr: kont(Var("x")) }),
      ]
      let args = args.map(fn(n) { self.to_js_var(n) })
      external(f, args_k + args) |> thunk
    }
  }
}

typealias Name = @knf.Name

typealias Type = @knf.Type

typealias Knf = @knf.Knf

pub struct JsLoweringCtx {
  knf : Knf
  tracer : @util.SubTracer
}

pub fn JsLoweringCtx::new(knf : Knf, tracer : @util.SubTracer) -> JsLoweringCtx {
  { knf, tracer }
}

pub fn JsLoweringCtx::lower(self : JsLoweringCtx) -> JsCore {
  let entry = JsFn::{
    name: "minimbt_main",
    params: [],
    expr: self.lower_expr(self.knf),
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
  JsFn::{ name: self.to_js_name(name), params, expr: self.lower_expr(body) }
}

fn JsLoweringCtx::lower_expr(self : JsLoweringCtx, knf : Knf) -> JsExpr {
  match knf {
    Knf::Unit => Lit("null")
    Int(i) => Lit(i.to_string())
    Double(d) => Lit(d.to_string())
    Neg(n) => UnOp("-", self.to_js_var(n))
    Add(l, r) => BinOp("+", self.to_js_var(l), self.to_js_var(r))
    Sub(l, r) => BinOp("-", self.to_js_var(l), self.to_js_var(r))
    Mul(l, r) => BinOp("*", self.to_js_var(l), self.to_js_var(r))
    Div(l, r) => BinOp("/", self.to_js_var(l), self.to_js_var(r))
    FNeg(n) => UnOp("-", self.to_js_var(n))
    FAdd(l, r) => BinOp("+", self.to_js_var(l), self.to_js_var(r))
    FSub(l, r) => BinOp("-", self.to_js_var(l), self.to_js_var(r))
    FMul(l, r) => BinOp("*", self.to_js_var(l), self.to_js_var(r))
    FDiv(l, r) => BinOp("/", self.to_js_var(l), self.to_js_var(r))
    IfEq(l, r, t, f) =>
      If(
        BinOp("==", self.to_js_var(l), self.to_js_var(r)),
        self.lower_expr(t),
        self.lower_expr(f),
      )
    IfLe(l, r, t, f) =>
      If(
        BinOp("<=", self.to_js_var(l), self.to_js_var(r)),
        self.lower_expr(t),
        self.lower_expr(f),
      )
    Var(n) => self.to_js_var(n)
    LetRec(f, b) => LetRec(self.lower_fn(f), self.lower_expr(b))
    Let((n, _), v, b) =>
      Let(self.to_js_name(n), self.lower_expr(v), self.lower_expr(b))
    LetTuple(xs, y, b) =>
      LetTuple(
        xs.map(fn(n) { self.to_js_name(n.0) }),
        self.to_js_var(y),
        self.lower_expr(b),
      )
    Get(a, i) => Get(self.to_js_var(a), self.to_js_var(i))
    Put(a, i, v) => Put(self.to_js_var(a), self.to_js_var(i), self.to_js_var(v))
    Apply(f, args) =>
      Call(self.to_js_var(f), args.map(fn(n) { self.to_js_var(n) }))
    Tuple(elems) => Tuple(elems.map(fn(n) { self.to_js_var(n) }))
    ExternalArray(_) => self.tracer.abort("ExternalArray not supported")
    ExternalFunctionApplication(f, args) =>
      Call(Var(f), args.map(fn(n) { self.to_js_var(n) }))
  }
}

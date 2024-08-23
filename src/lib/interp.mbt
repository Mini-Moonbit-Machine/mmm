enum Kind {
  Unit
  Bool(Bool)
  Int(Int)
  Tuple(Int)
  Named(String)
  Defn(Defn)
  Closure(Array[(String, Value)])
} derive(Show)

struct Value {
  kind : Kind
  fields : Array[Value]
} derive(Show)

fn op_equal(self : Value, other : Value) -> Bool {
  match (self.kind, other.kind) {
    (Unit, Unit) => true
    (Bool(x), Bool(y)) => x == y
    (Int(x), Int(y)) => x == y
    (Tuple(n), Tuple(m)) => n == m && self.fields == other.fields
    (Named(x), Named(y)) => x == y && self.fields == other.fields
    _ => false
  }
}

fn Value::unit() -> Value {
  { kind: Unit, fields: Array::new() }
}

fn Value::bool(x : Bool) -> Value {
  { kind: Bool(x), fields: Array::new() }
}

fn Value::int(x : Int) -> Value {
  { kind: Int(x), fields: Array::new() }
}

fn Value::tuple(n : Int, xs : Array[Value]) -> Value {
  { kind: Tuple(n), fields: xs }
}

fn Value::named(x : String, xs : Array[Value]) -> Value {
  { kind: Named(x), fields: xs }
}

fn Value::defn(x : Defn) -> Value {
  { kind: Defn(x), fields: Array::new() }
}

fn Value::closure(x : Defn, fvs : Array[(String, Value)]) -> Value {
  { kind: Closure(fvs), fields: [Value::defn(x)] }
}

typealias Env = @immut/hashmap.T[String, Value]

pub struct Interpreter {
  env : Env
} derive(Show)

pub fn Interpreter::new() -> Interpreter {
  { env: Env::new() }
}

pub fn Interpreter::new_with_env(env : Env) -> Interpreter {
  { env, }
}

pub fn eval(self : Interpreter, x : Node) -> Value!Failure {
  match x {
    Unit => Value::unit()
    Bool(x) => Value::bool(x)
    Int(x) => Value::int(x)
    Var(x) =>
      match self.env.find(x) {
        Some(v) => v
        None => fail!("unbound variable: \{x}")
      }
    Tuple(xs) => {
      let ys = Array::new()
      for x in xs {
        ys.push(self.eval!(x))
      }
      Value::tuple(xs.length(), ys)
    }
    Op(op, x, y) => {
      let x = self.eval!(x)
      let y = self.eval!(y)
      match (op, x, y) {
        (Add, { kind: Int(x), .. }, { kind: Int(y), .. }) => Value::int(x + y)
        (Sub, { kind: Int(x), .. }, { kind: Int(y), .. }) => Value::int(x - y)
        (Mul, { kind: Int(x), .. }, { kind: Int(y), .. }) => Value::int(x * y)
        (Div, { kind: Int(x), .. }, { kind: Int(y), .. }) => Value::int(x / y)
        (Eq, x, y) => Value::bool(x == y)
        _ => fail!("invalid operation")
      }
    }
    App(f, xs) => {
      let f = self.eval!(f)
      let ys = Array::new()
      for x in xs {
        ys.push(self.eval!(x))
      }
      match f {
        { kind: Closure(fvs), fields: [{ kind: Defn(f), .. }] } => {
          let mut env = self.env
          if f.params.length() != ys.length() {
            fail!("invalid number of arguments")
          }
          for binding in fvs.iter() {
            let (v, x) = binding
            env = env.add(v, x)
          }
          for i, v in f.params.iter2() {
            env = env.add(v, ys[i])
          }
          let interp = Interpreter::new_with_env(env)
          interp.eval!(f.body)
        }
        { kind: Defn(f), .. } => {
          let mut env = self.env
          if f.params.length() != ys.length() {
            fail!("invalid number of arguments")
          }
          for i, v in f.params.iter2() {
            env = env.add(v, ys[i])
          }
          let interp = Interpreter::new_with_env(env)
          interp.eval!(f.body)
        }
        _ => fail!("invalid closure")
      }
    }
    CApp(op, xs) => {
      let ys = Array::new()
      for x in xs {
        ys.push(self.eval!(x))
      }
      match op {
        _ => fail!("not implemented")
      }
    }
    Get(x, i) => {
      let x = self.eval!(x)
      let i = self.eval!(i)
      match (x, i) {
        ({ kind: Tuple(_) | Named(_), fields: xs }, { kind: Int(i), .. }) =>
          xs[i]
        ({ kind: Tuple(_) | Named(_), .. }, _) => fail!("invalid index")
        (_, { kind: Int(_), .. }) => fail!("invalid data to index")
        _ => fail!("invalid operation")
      }
    }
    If(e, c1, c2) => {
      let x = self.eval!(e)
      match x {
        { kind: Bool(true), .. } => self.eval!(c1)
        { kind: Bool(false), .. } => self.eval!(c2)
        _ => fail!("invalid condition")
      }
    }
    Let(x, e1, e2) => {
      let v = self.eval!(e1)
      let env = x.map_or_else(fn() { self.env }, fn(it) { self.env.add(it, v) })
      let interp = Interpreter::new_with_env(env)
      interp.eval!(e2)
    }
    LetRec(fs, e) => {
      let mut env = self.env
      for f in fs {
        let fvs = free_var_of_defn(f)
        if fvs.is_empty() {
          let v = Value::defn(f)
          env = env.add(f.name, v)
        } else {
          let fv_map = Array::new()
          for fv in fvs {
            let x = self.eval!(Var(fv))
            fv_map.push((fv, x))
          }
          let v = Value::closure(f, fv_map)
          env = env.add(f.name, v)
        }
      }
      let interp = Interpreter::new_with_env(env)
      interp.eval!(e)
    }
    Put(x, i, y) => {
      let x = self.eval!(x)
      let i = self.eval!(i)
      let y = self.eval!(y)
      match (x, i) {
        ({ kind: Tuple(_) | Named(_), fields: xs }, { kind: Int(i), .. }) =>
          xs[i] = y
        ({ kind: Tuple(_) | Named(_), .. }, _) => fail!("invalid index")
        (_, { kind: Int(_), .. }) => fail!("invalid data to index")
        _ => fail!("invalid operation")
      }
      Value::unit()
    }
  }
}

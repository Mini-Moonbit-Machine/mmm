pub typealias LocalEnv = @immut/hashmap.T[String, Type]

pub typealias NameEnv = @immut/hashmap.T[String, Name]

pub struct TypeClassEnv {
  classes : @immut/hashmap.T[String, @types.Classdef] // class name -> class
  instances : Array[((String, Type), Name)] // (class name, instance type) -> dict name
  // method name -> (dict name, dict size, method type, method index)
  methods : @immut/hashmap.T[String, (String, Int, @types.Type, Int)]
  function_dicts : @immut/hashmap.T[String, Array[(String, String)]] // function name -> [type var with a dict, class name]
}

pub fn TypeClassEnv::new() -> TypeClassEnv {
  {
    classes: @immut/hashmap.new(),
    instances: [],
    methods: @immut/hashmap.new(),
    function_dicts: @immut/hashmap.new(),
  }
}

fn TypeClassEnv::add_class(
  self : TypeClassEnv,
  class : @types.Classdef
) -> TypeClassEnv {
  { ..self, classes: self.classes.add(class.name, class) }
}

fn TypeClassEnv::add_instance(
  self : TypeClassEnv,
  class : String,
  ty : Type,
  dict : Name
) -> TypeClassEnv {
  let instances = self.instances
  if instances.search_by(fn(x) { x.0 == (class, ty) }).is_empty() == false {
    @util.t.abort("duplicate instance: \{ty} \{class}")
  }
  instances.push(((class, ty), dict))
  { ..self, instances, }
}

fn TypeClassEnv::add_method(
  self : TypeClassEnv,
  name : String,
  dict : String,
  size : Int,
  ty : Type,
  index : Int
) -> TypeClassEnv {
  { ..self, methods: self.methods.add(name, (dict, size, ty, index)) }
}

fn TypeClassEnv::add_function_dict(
  self : TypeClassEnv,
  name : String,
  dict_type_names : Array[(String, String)]
) -> TypeClassEnv {
  { ..self, function_dicts: self.function_dicts.add(name, dict_type_names) }
}

fn TypeClassEnv::get_instance(
  self : TypeClassEnv,
  class : String,
  ty : Type
) -> Name? {
  self.instances
  .search_by(fn(x) { x.0 == (class, ty) })
  .map(fn(x) { self.instances[x].1 })
}

pub typealias BindEnv = @immut/hashmap.T[Occurrence, (Name, Type)]

struct KnfEnv {
  mut counter : Int
  externals : LocalEnv
  tracer : @util.SubTracer
}

pub fn KnfEnv::new(
  externals : @immut/hashmap.T[String, Type],
  tracer : @util.SubTracer
) -> KnfEnv {
  { counter: 1, externals, tracer }
}

pub fn KnfEnv::init_counter_from_existing(
  self : KnfEnv,
  existing : Knf
) -> Unit {
  self.counter = knf_max_counter(existing) + 1
}

fn KnfEnv::new_temp(self : KnfEnv) -> Name {
  let temp = Name::slot_only(self.counter)
  self.counter += 1
  temp
}

fn KnfEnv::new_named_temp(self : KnfEnv, name : Name) -> Name {
  let counter = self.counter
  self.counter += 1
  { ..name, slot: counter }
}

pub fn KnfEnv::to_knf(self : KnfEnv, s : @types.Syntax) -> Knf!Failure {
  let let_list = []
  let name_env = NameEnv::new()
  // externals are handled by @typer.externals()
  let local_env = LocalEnv::new()
  let typeclass_env = TypeClassEnv::new()
  let r = self.build_knf!(s, let_list, name_env, local_env, typeclass_env)
  let knf = bind_all(r)
  self.tracer.record("knf", knf.to_pretty_print())
  knf
}

enum Binding {
  Let(Name, Type, Knf)
  LetRec(FuncDef)
  LetTuple(Array[(Name, Type)], Name)
  LetUnboxedTuple(Array[(Name, Type)], Name)
}

typealias LetList = Array[Binding]

typealias KnfResult = (Knf, Type, LetList)

fn bind_all(kr : KnfResult) -> Knf {
  let (k, _, let_list) = kr
  let mut ret = k
  let rlist = let_list.rev()
  for item in rlist {
    ret = match item {
      Binding::Let(x, ty, e) => Let((x, ty), e, ret)
      Binding::LetRec(f) => LetRec(f, ret)
      Binding::LetTuple(xs, y) => LetTuple(xs, y, ret)
      Binding::LetUnboxedTuple(xs, y) => LetUnboxedTuple(xs, y, ret)
    }
  }
  ret
}

fn materialize_knf(self : KnfEnv, knf : KnfResult, let_list : LetList) -> Name {
  match knf {
    (Knf::Var(x), _, _) => x
    (k, ty, _) => {
      let temp = self.new_temp()
      let_list.push(Binding::Let(temp, ty, k))
      temp
    }
  }
}

fn materialize_knf_with_name(
  self : KnfEnv,
  knf : KnfResult,
  let_list : LetList,
  name : String
) -> Name!Failure {
  match knf {
    (Knf::Var(x), _, _) => x
    (k, ty, _) => {
      let temp = self.new_named_temp(
        try {
          if name == "_" {
            self.new_temp()
          } else {
            Name::from_string!(name)
          }
        } catch {
          _ => fail!("invalid variable name: \{name}")
        },
      )
      let_list.push(Binding::Let(temp, ty, k))
      temp
    }
  }
}

fn make_name(self : KnfEnv, name : String) -> Name {
  if name == "_" {
    self.new_temp()
  } else {
    let new_id = self.counter
    self.counter += 1
    Name::name_and_slot(name, new_id)
  }
}

fn pattern_match_fail() -> Knf {
  ExternalFunctionApplication("minimbt_panic", [])
}

fn fold_switch_tree_to_knf(
  self : KnfEnv,
  occur : Occurrence,
  rev_cases : Array[(SubPatSpec, DecisionTree)],
  default : DecisionTree?,
  let_list : LetList,
  bind_env : BindEnv,
  env : NameEnv,
  ty_env : LocalEnv,
  tc_env : TypeClassEnv
) -> KnfResult!Failure {
  let case = rev_cases.pop()
  match case {
    None =>
      match default {
        Some(default) =>
          self.decision_tree_to_knf!(
            default, let_list, bind_env, env, ty_env, tc_env,
          )
        None => (pattern_match_fail(), Unit, let_list) // TODO: check exhaustiveness
      }
    Some((subpat, subtree)) => {
      let mut bind_env2 = bind_env
      fn tree_check_lit_to_knf(
        lit : @types.Syntax,
        subtree : DecisionTree
      ) -> KnfResult!Failure {
        let x = match bind_env.find(occur) {
          Some(x) => x
          None => fail!("unbound occurrence: \{occur}")
        }
        let r = self.build_knf!(lit, let_list, env, ty_env, tc_env)
        let o = self.materialize_knf(r, let_list)
        let subtreek = self.decision_tree_to_knf!(
          subtree,
          [],
          bind_env,
          env,
          ty_env,
          tc_env,
        )
        let rest = self.fold_switch_tree_to_knf!(
          occur,
          rev_cases,
          default,
          [],
          bind_env,
          env,
          ty_env,
          tc_env,
        )
        (IfEq(x.0, o, bind_all(subtreek), bind_all(rest)), subtreek.1, let_list)
      }

      match subpat {
        Unit => tree_check_lit_to_knf!(Unit, subtree)
        BoolLit(~bval) => tree_check_lit_to_knf!(Bool(bval), subtree)
        IntLit(~ival) => tree_check_lit_to_knf!(Int(ival), subtree)
        DoubleLit(~dval) => tree_check_lit_to_knf!(Double(dval), subtree)
        StringLit(~strval) => tree_check_lit_to_knf!(String(strval), subtree)
        Ctor(~name, ~csize) => {
          // unpack occurrences and bind name
          let ctor_ty = ty_env
            .find(name)
            .or_else(fn() { self.tracer.abort("not a ctor") })
          guard let Type::Fun(_, ty) = ctor_ty.repr() else {
            _ => fail!("ctor type \{ctor_ty} is not a function type")
          }
          guard let Type::Enum(_, { val: Some(variants) }) = ty.repr() else {
            _ => fail!("ctor type \{ty} is not an enum type")
          }
          let variant_id = variants
            .search_by(fn(ctor) { name == ctor.0 })
            .unwrap()
          let (_, args_ty) = variants[variant_id]
          if args_ty.length() != csize {
            fail!("unmatched ctor size: \{name} \{csize}")
          }
          let x = match bind_env.find(occur) {
            Some(x) => x
            None => fail!("unbound occurrence: \{occur}")
          }
          // ctor id: variant index
          let r = self.build_knf!(
            Int(variant_id.to_int64()),
            let_list,
            env,
            ty_env,
            tc_env,
          )
          let o = self.materialize_knf(r, let_list)
          // unpack tag
          let tag_tmp = self.new_temp()
          let args_tmp = self.new_temp()
          let_list.push(
            Binding::LetTuple([(tag_tmp, Int), (args_tmp, Tuple(args_ty))], x.0),
          )
          // unpack ctor arguments
          let new_binders = []
          for i, arg_ty in args_ty {
            let tmp = self.new_temp()
            let suboccur = occur.destruct(i)
            new_binders.push((tmp, arg_ty))
            bind_env2 = bind_env2.add(suboccur, (tmp, arg_ty))
          }
          let subtreek = self.decision_tree_to_knf!(
            subtree,
            [Binding::LetTuple(new_binders, args_tmp)],
            bind_env2,
            env,
            ty_env,
            tc_env,
          )
          let rest = self.fold_switch_tree_to_knf!(
            occur,
            rev_cases,
            default,
            [],
            bind_env,
            env,
            ty_env,
            tc_env,
          )
          (
            IfEq(tag_tmp, o, bind_all(subtreek), bind_all(rest)),
            subtreek.1,
            let_list,
          )
        }
        Tuple(~tsize) => {
          // unpack occurrences and bind name
          let x = match bind_env.find(occur) {
            Some(x) => x
            None => fail!("unbound occurrence: \{occur}")
          }
          let new_binders = []
          for i in 0..<tsize {
            let tmp = self.new_temp()
            let dyn_ty = Type::Var(Ref::new(None))
            let suboccur = occur.destruct(i)
            new_binders.push((tmp, dyn_ty))
            bind_env2 = bind_env2.add(suboccur, (tmp, dyn_ty))
          }
          let_list.push(Binding::LetTuple(new_binders, x.0))
          let subtreek = self.decision_tree_to_knf!(
            subtree, let_list, bind_env2, env, ty_env, tc_env,
          )
          subtreek
        }
      }
    }
  }
}

fn decision_tree_to_knf(
  self : KnfEnv,
  tree : DecisionTree,
  let_list : LetList,
  bind_env : BindEnv,
  env : NameEnv,
  ty_env : LocalEnv,
  tc_env : TypeClassEnv
) -> KnfResult!Failure {
  self.tracer.println("decision_tree_to_knf: \{tree.to_pretty_print()}")
  match tree {
    Succeed(~binds, ~action) => {
      let mut env2 = env
      let mut ty_env2 = ty_env
      for occnm in binds {
        let (occ, name) = occnm
        let (knf_name, ty) = match bind_env.find(occ) {
          Some(x) => x
          None => fail!("unbound occurrence: \{occ}")
        }
        env2 = env2.add(name, knf_name)
        ty_env2 = ty_env2.add(name, ty)
      }
      let r = self.build_knf!(action, let_list, env2, ty_env2, tc_env)
      r
    }
    Fail => (pattern_match_fail(), Unit, let_list)
    Switch(~occur, ~cases, ~default) => {
      let rev_cases = cases.rev()
      self.fold_switch_tree_to_knf!(
        occur, rev_cases, default, let_list, bind_env, env, ty_env, tc_env,
      )
    }
  }
}

fn build_knf(
  self : KnfEnv,
  node : @types.Syntax,
  let_list : LetList,
  env : NameEnv,
  ty_env : LocalEnv,
  tc_env : TypeClassEnv
) -> KnfResult!Failure {
  match node {
    Unit => (Unit, Unit, let_list)
    Int(i) => (Int(i), Int, let_list)
    Neg(Int(i), ..) => (Int(-i), Int, let_list)
    Bool(b) => (Bool(b), Bool, let_list)
    Double(d) => (Double(d), Double, let_list)
    String(s) => (String(s), String, let_list)
    Var(x) =>
      match (env.find(x), ty_env.find(x)) {
        (Some(name), Some(ty)) => (Var(name), ty, let_list)
        _ =>
          match tc_env.methods.find(x) {
            Some((dict, size, ty, index)) =>
              match (env.find(dict), ty_env.find(dict)) {
                (Some(dict_name), Some(dict_type)) => {
                  guard let Type::Tuple(args) = dict_type else {
                    _ => fail!("not a tuple: \{dict_type}")
                  }
                  let tmps = []
                  for i in 0..<size {
                    let tmp = self.new_temp()
                    tmps.push((tmp, args[i]))
                  }
                  let_list.push(Binding::LetTuple(tmps, dict_name))
                  (Var(tmps[index].0), ty, let_list)
                }
                _ => fail!("unbound dict: \{dict}")
              }
            None => fail!("unbound variable: \{x}")
          }
      }
    Tuple(xs) => {
      let ys = []
      let tys = []
      for x in xs {
        let r = self.build_knf!(x, let_list, env, ty_env, tc_env)
        let tmp = self.materialize_knf(r, let_list)
        ys.push(tmp)
        tys.push(r.1)
      }
      (Tuple(ys), Tuple(tys), let_list)
    }
    Not(xs) => {
      let r1 = self.build_knf!(xs, let_list, env, ty_env, tc_env)
      let tmp = self.materialize_knf(r1, let_list)
      let r2 = self.build_knf!(Bool(true), let_list, env, ty_env, tc_env)
      let const_1 = self.materialize_knf(r2, let_list)
      (IfEq(tmp, const_1, Bool(false), Bool(true)), Int, let_list)
    }
    Array(len, elem) => {
      let r1 = self.build_knf!(len, let_list, env, ty_env, tc_env)
      let len = self.materialize_knf(r1, let_list)
      let r2 = self.build_knf!(elem, let_list, env, ty_env, tc_env)
      let elem = self.materialize_knf(r2, let_list)
      let call = match r2.1.repr() {
        Int | Bool =>
          ExternalFunctionApplication("minimbt_create_array", [len, elem])
        Double =>
          ExternalFunctionApplication("minimbt_create_float_array", [len, elem])
        t =>
          if t.is_float_like() {
            ExternalFunctionApplication(
              "minimbt_create_float_array",
              [len, elem],
            )
          } else if t.is_ptr_like() {
            ExternalFunctionApplication("minimbt_create_ptr_array", [len, elem])
          } else {
            fail!("invalid array element type: \{r2.1}")
          }
      }
      (call, Array(r2.1), let_list)
    }
    Neg(xs, kind=Some(@types.Kind::Int)) => {
      let r = self.build_knf!(xs, let_list, env, ty_env, tc_env)
      let tmp = self.materialize_knf(r, let_list)
      (Neg(tmp), Int, let_list)
    }
    Neg(xs, kind=Some(@types.Kind::Double)) => {
      let r = self.build_knf!(xs, let_list, env, ty_env, tc_env)
      let tmp = self.materialize_knf(r, let_list)
      (FNeg(tmp), Double, let_list)
    }
    Neg(_, ~kind) => fail!("invalid kind: \{kind}")
    App(fv, ~targs, xs) => {
      fn fallback(f : String?) -> KnfResult!Failure {
        let r = self.build_knf!(fv, let_list, env, ty_env, tc_env)
        let fv = self.materialize_knf(r, let_list)
        let args = []
        match f {
          Some(f) =>
            match (tc_env.function_dicts.find(f), targs) {
              (Some(dict_type_names), Some(targs)) =>
                for abstract_ty in dict_type_names {
                  let (abstract_ty, class) = abstract_ty
                  let real_ty_idx = targs
                    .search_by(fn(t) { t.0 == abstract_ty })
                    .or_else(
                      fn() { self.tracer.abort("type argument not found") },
                    )
                  let (_, real_ty) = targs[real_ty_idx]
                  let instance = tc_env
                    .get_instance(class, real_ty)
                    .or_else(
                      fn() {
                        self.tracer.abort(
                          "instance not found: \{class} \{real_ty} in \{tc_env.instances}",
                        )
                      },
                    )
                  args.push(instance)
                }
              (Some(_), None) =>
                fail!(
                  "type arguments are required for dictionary passing: calling \{f}",
                )
              _ => ()
            }
          None =>
            if targs.is_empty() == false {
              fail!("type arguments are not supported for high order functions")
            }
        }
        for x in xs {
          let r = self.build_knf!(x, let_list, env, ty_env, tc_env)
          let tmp = self.materialize_knf(r, let_list)
          args.push(tmp)
        }
        (
          Apply(fv, args),
          match r.1.repr() {
            @types.Type::Fun(_, t) => t
            _ => fail!("not a function: \{r.0} : \{r.1}")
          },
          let_list,
        )
      }

      match fv {
        Var(f) =>
          if self.externals.find(f).is_empty() == false {
            let args = []
            for x in xs {
              let r = self.build_knf!(x, let_list, env, ty_env, tc_env)
              let tmp = self.materialize_knf(r, let_list)
              args.push(tmp)
            }
            (
              ExternalFunctionApplication("minimbt_" + f, args),
              match self.externals.find(f).unwrap() {
                @types.Type::Fun(_, t) => t
                _ =>
                  fail!(
                    "not a function: \{fv} : \{self.externals.find(f).unwrap()}",
                  )
              },
              let_list,
            )
          } else {
            fallback!(Some(f))
          }
        _ => fallback!(None)
      }
    }
    Get(x, y) => {
      let r1 = self.build_knf!(x, let_list, env, ty_env, tc_env)
      let x = self.materialize_knf(r1, let_list)
      let r2 = self.build_knf!(y, let_list, env, ty_env, tc_env)
      let y = self.materialize_knf(r2, let_list)
      (
        Get(x, y),
        match r1.1.repr() {
          @types.Type::Array(t) => t
          _ => fail!("not a array: \{r1.0} : \{r1.1}")
        },
        let_list,
      )
    }
    // It's possible to omit the 3 cases, but it's better to fuse them here
    If(Eq(o1, o2), y, z) => {
      let r1 = self.build_knf!(o1, let_list, env, ty_env, tc_env)
      let o1 = self.materialize_knf(r1, let_list)
      let r2 = self.build_knf!(o2, let_list, env, ty_env, tc_env)
      let o2 = self.materialize_knf(r2, let_list)
      let ll1 = []
      let r3 = self.build_knf!(y, ll1, env, ty_env, tc_env)
      let ll2 = []
      let r4 = self.build_knf!(z, ll2, env, ty_env, tc_env)
      (IfEq(o1, o2, bind_all(r3), bind_all(r4)), r3.1, let_list)
    }
    If(LE(o1, o2), y, z) => {
      let r1 = self.build_knf!(o1, let_list, env, ty_env, tc_env)
      let o1 = self.materialize_knf(r1, let_list)
      let r2 = self.build_knf!(o2, let_list, env, ty_env, tc_env)
      let o2 = self.materialize_knf(r2, let_list)
      let ll1 = []
      let r3 = self.build_knf!(y, ll1, env, ty_env, tc_env)
      let ll2 = []
      let r4 = self.build_knf!(z, ll2, env, ty_env, tc_env)
      (IfLe(o1, o2, bind_all(r3), bind_all(r4)), r3.1, let_list)
    }
    If(Not(x), y, z) =>
      self.build_knf!(If(x, z, y), let_list, env, ty_env, tc_env)
    If(x, y, z) => {
      let r = self.build_knf!(x, let_list, env, ty_env, tc_env)
      let x = self.materialize_knf(r, let_list)
      let ll1 = []
      let r1 = self.build_knf!(y, ll1, env, ty_env, tc_env)
      let ll2 = []
      let r2 = self.build_knf!(z, ll2, env, ty_env, tc_env)
      let r3 = self.build_knf!(Bool(true), let_list, env, ty_env, tc_env)
      let const_1 = self.materialize_knf(r3, let_list)
      (IfEq(x, const_1, bind_all(r1), bind_all(r2)), r1.1, let_list)
    }
    Prim(o1, o2, op, kind=Some(@types.Kind::Int)) => {
      let r1 = self.build_knf!(o1, let_list, env, ty_env, tc_env)
      let x = self.materialize_knf(r1, let_list)
      let r2 = self.build_knf!(o2, let_list, env, ty_env, tc_env)
      let y = self.materialize_knf(r2, let_list)
      (
        match op {
          Add => Add(x, y)
          Sub => Sub(x, y)
          Mul => Mul(x, y)
          Div => Div(x, y)
        },
        Int,
        let_list,
      )
    }
    Prim(o1, o2, op, kind=Some(@types.Kind::Double)) => {
      let r1 = self.build_knf!(o1, let_list, env, ty_env, tc_env)
      let x = self.materialize_knf(r1, let_list)
      let r2 = self.build_knf!(o2, let_list, env, ty_env, tc_env)
      let y = self.materialize_knf(r2, let_list)
      (
        match op {
          Add => FAdd(x, y)
          Sub => FSub(x, y)
          Mul => FMul(x, y)
          Div => FDiv(x, y)
        },
        Double,
        let_list,
      )
    }
    Prim(_, _, _, ~kind) => fail!("invalid kind: \{kind}")
    Eq(o1, o2) => {
      let r1 = self.build_knf!(o1, let_list, env, ty_env, tc_env)
      let x = self.materialize_knf(r1, let_list)
      let r2 = self.build_knf!(o2, let_list, env, ty_env, tc_env)
      let y = self.materialize_knf(r2, let_list)
      (IfEq(x, y, Bool(true), Bool(false)), Int, let_list)
    }
    LE(o1, o2) => {
      let r1 = self.build_knf!(o1, let_list, env, ty_env, tc_env)
      let x = self.materialize_knf(r1, let_list)
      let r2 = self.build_knf!(o2, let_list, env, ty_env, tc_env)
      let y = self.materialize_knf(r2, let_list)
      (IfLe(x, y, Bool(true), Bool(false)), Int, let_list)
    }
    Let((x, ty), e1, e2) => {
      let r1 = self.build_knf!(e1, let_list, env, ty_env, tc_env)
      let e1 = self.materialize_knf_with_name!(r1, let_list, x)
      let env = env.add(x, e1)
      let ty_env = ty_env.add(x, ty)
      let r2 = self.build_knf!(e2, let_list, env, ty_env, tc_env)
      (r2.0, r2.1, let_list)
    }
    CtorApp(f, xs) => {
      let ctor_ty = ty_env
        .find(f)
        .or_else(fn() { self.tracer.abort("not a ctor") })
      guard let Type::Fun(_, ty) = ctor_ty.repr() else {
        _ => fail!("ctor type \{ctor_ty} is not a function type")
      }
      guard let Type::Enum(_, { val: Some(variants) }) = ty.repr() else {
        _ => fail!("ctor type \{ty} is not an enum type")
      }
      let variant_id = variants.search_by(fn(ctor) { f == ctor.0 }).unwrap()
      let idk = self.build_knf!(
        Int(variant_id.to_int64()),
        let_list,
        env,
        ty_env,
        tc_env,
      )
      let idn = self.materialize_knf(idk, let_list)
      let args = []
      let tys = []
      for x in xs {
        let r = self.build_knf!(x, let_list, env, ty_env, tc_env)
        let tmp = self.materialize_knf(r, let_list)
        args.push(tmp)
        tys.push(r.1)
      }
      // indirection: Tuple(tag, Tuple(..args))
      let argk = (Knf::Tuple(args), @types.Type::Tuple(tys), let_list)
      let argn = self.materialize_knf(argk, let_list)
      (Tuple([idn, argn]), ty, let_list)
    }
    Match(e, _) as matcher => {
      let r = self.build_knf!(e, let_list, env, ty_env, tc_env)
      let scrutinee = self.materialize_knf(r, let_list)
      let tree = PatternMatcher::new(matcher, self.tracer).run()
      let bind_env = BindEnv::of([(Occurrence::empty(), (scrutinee, r.1))])
      let r = self.decision_tree_to_knf!(
        tree, let_list, bind_env, env, ty_env, tc_env,
      )
      r
    }
    LetEnum(_, ty, e) => {
      let mut ty_env = ty_env
      guard let Type::Enum(_, { val: Some(variants) }) = ty.repr() else {
        _ => fail!("infer: \{ty} is not an enum type")
      }
      for variant in variants {
        let (cname, args_ty) = variant
        // Add constructors to type scheme environment.
        // This is for constructors such as 'None' and 'Nil'.
        let variant_ty = Type::Fun(args_ty, ty)
        ty_env = ty_env.add(cname, variant_ty)
      }
      self.build_knf!(e, let_list, env, ty_env, tc_env)
    }
    LetRec({ name, args, body, tyvars }, e) => {
      let dict_args = []
      let dict_tys = []
      let mut tc_env = tc_env
      let tvars_with_contraints = []
      let mem_class = @hashset.new()
      for tvar in tyvars {
        match tvar.1 {
          Some(tc) =>
            match tc_env.classes.find(tc) {
              Some(class) => {
                if mem_class.contains(tc) {
                  continue
                }
                mem_class.insert(tc)
                let dict_name = self.make_name("dict")
                let tys = []
                for j, method in class.methods {
                  let (method_name, method_ty) = method
                  tc_env = tc_env.add_method(
                    method_name,
                    dict_name.to_string(),
                    class.methods.length(),
                    method_ty,
                    j,
                  )
                  tys.push(method_ty)
                }
                tvars_with_contraints.push((tvar.0, tc))
                dict_args.push(dict_name.to_string())
                dict_tys.push(Type::Tuple(tys))
              }
              None => fail!("undefined type class: \{tc}")
            }
          None => ()
        }
      }
      if tvars_with_contraints.is_empty() == false {
        tc_env = tc_env.add_function_dict(name.0, tvars_with_contraints)
      }
      let ty = name.1
      let orig_name = name.0
      let name = self.make_name(orig_name)
      let mut env = env
      let mut ty_env = ty_env
      env = env.add(orig_name, name)
      ty_env = ty_env.add(
        orig_name,
        @types.Type::Fun(args.map(fn(x) { x.1 }), ty),
      )
      let mut env2 = env
      let mut ty_env2 = ty_env
      let new_binders = []
      let inner_let_list = []
      for i, dict_arg in dict_args {
        let dict_ty = dict_tys[i]
        let dict_name = try {
          Name::from_string!(dict_arg)
        } catch {
          _ => fail!("invalid dict name: \{dict_arg}")
        }
        new_binders.push((dict_name, dict_ty))
        ty_env2 = ty_env2.add(dict_arg, dict_ty)
        env2 = env2.add(dict_arg, dict_name)
      }
      for item in args {
        let (x, ty) = item
        let x_name = self.make_name(x)
        let tmp = self.new_named_temp(x_name)
        new_binders.push((tmp, ty))
        env2 = env2.add(x, tmp)
        ty_env2 = ty_env2.add(x, ty)
      }
      let r = self.build_knf!(body, inner_let_list, env2, ty_env2, tc_env)
      let new_def = FuncDef::{ name, ty, args: new_binders, body: bind_all(r) }
      let_list.push(Binding::LetRec(new_def))
      let r = self.build_knf!(e, let_list, env, ty_env, tc_env)
      (r.0, r.1, let_list)
    }
    LetTuple(xs, y, e) => {
      let r1 = self.build_knf!(y, let_list, env, ty_env, tc_env)
      let y = self.materialize_knf(r1, let_list)
      let mut env2 = env
      let mut ty_env2 = ty_env
      let new_binders = []
      for item in xs {
        let (x, ty) = item
        let x_name = self.make_name(x)
        let tmp = self.new_named_temp(x_name)
        new_binders.push((tmp, ty))
        env2 = env2.add(x, tmp)
        ty_env2 = ty_env2.add(x, ty)
      }
      let_list.push(Binding::LetTuple(new_binders, y))
      let r2 = self.build_knf!(e, let_list, env2, ty_env2, tc_env)
      (r2.0, r2.1, let_list)
    }
    Put(dst, idx, src) => {
      let r1 = self.build_knf!(dst, let_list, env, ty_env, tc_env)
      let dst = self.materialize_knf(r1, let_list)
      let r2 = self.build_knf!(idx, let_list, env, ty_env, tc_env)
      let idx = self.materialize_knf(r2, let_list)
      let r3 = self.build_knf!(src, let_list, env, ty_env, tc_env)
      let src = self.materialize_knf(r3, let_list)
      (Put(dst, idx, src), Unit, let_list)
    }
    LetClass(classdef, e) => {
      let tc_env = tc_env.add_class(classdef)
      self.build_knf!(e, let_list, env, ty_env, tc_env)
    }
    LetInstance({ name, methods, ty }, e) => {
      let xs = []
      let tys = []
      for method in methods {
        let (_, method_body) = method
        let r = self.build_knf!(method_body, let_list, env, ty_env, tc_env)
        let x = self.materialize_knf(r, let_list)
        xs.push(x)
        tys.push(r.1)
      }
      let dict_name = self.new_named_temp(Name::name_only("D$\{name}"))
      let_list.push(Binding::Let(dict_name, Type::Tuple(tys), Tuple(xs)))
      let tc_env = tc_env.add_instance(name, ty, dict_name)
      self.build_knf!(e, let_list, env, ty_env, tc_env)
    }
  }
}

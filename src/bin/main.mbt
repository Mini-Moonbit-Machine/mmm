/// The stage to be run next.
enum Stages {
  Parse
  Typecheck
  Knf
  KnfOpt
  Js
  Closure
  Core
  CoreOpt
  Wasm
  Asm
  Finished
} derive(Show, Eq, Compare)

fn Stages::from_string(s : String) -> Stages? {
  match s {
    "parse" => Some(Stages::Parse)
    "typecheck" => Some(Stages::Typecheck)
    "knf" => Some(Stages::Knf)
    "knf-opt" => Some(Stages::KnfOpt)
    "js" => Some(Stages::Js)
    "core" => Some(Stages::Core)
    "core-opt" => Some(Stages::CoreOpt)
    "wasm" => Some(Stages::Wasm)
    "closure" => Some(Stages::Closure)
    "riscv" => Some(Stages::Asm)
    "finished" => Some(Stages::Finished)
    _ => None
  }
}

fn Stages::next(self : Stages) -> Stages {
  match self {
    Stages::Parse => Stages::Typecheck
    Stages::Typecheck => Stages::Knf
    Stages::Knf => Stages::KnfOpt
    Stages::KnfOpt => Stages::Js
    Stages::Js => Stages::Closure
    Stages::Closure => Stages::Core
    Stages::Core => Stages::CoreOpt
    Stages::CoreOpt => Stages::Wasm
    Stages::Wasm => Stages::Asm
    Stages::Asm => Stages::Finished
    Stages::Finished => Stages::Finished
  }
}

let tracer : @util.Tracer = @util.tracer

let mt : @util.SubTracer = @util.t

let ot : @util.SubTracer = @util.output

let opts : Options = Options::new()

type Flags @hashset.T[String]

fn Flags::has(self : Flags, flag : String) -> Bool {
  self._.contains(flag)
}

struct CompileStatus {
  mut flags : Flags
  mut curr_stage : Stages
  end_stage : Stages
  mut source_code : String?
  mut ast : @types.Syntax?
  mut typechecked : @types.Syntax?
  mut knf : @knf.Knf?
  mut opt_knf : @knf.Knf?
  mut js : @lower_js.JsCore?
  mut core : @core.Core?
  mut opt_core : @core.Core?
  mut wasm : @lower_wasm.MCore?
  mut closure_ir : @closure.Program?
  mut asm : @lower_riscv.MCore?
  mut knf_env : @knf.KnfEnv?
  mut core_env : @core.CoreEnv?
}

fn CompileStatus::initialize(
  start_stage : Stages,
  end_stage : Stages,
  file : String
) -> CompileStatus! {
  let v = CompileStatus::{
    flags: @hashset.new(),
    curr_stage: start_stage,
    end_stage,
    source_code: None,
    ast: None,
    typechecked: None,
    knf: None,
    opt_knf: None,
    js: None,
    wasm: None,
    closure_ir: None,
    asm: None,
    core: None,
    opt_core: None,
    knf_env: None,
    core_env: None,
  }
  match start_stage {
    Parse => v.source_code = Some(file)
    _ => fail!("invalid start stage")
  }
  v
}

/// Run the next stage of compilation. Returns true if the compilation is finished.
fn CompileStatus::step(self : CompileStatus) -> Bool {
  if self.curr_stage >= self.end_stage {
    return true
  }
  match self.curr_stage {
    Parse => {
      let source_code = self.source_code.unwrap()
      let subtracer = tracer.new_subtracer(
        "Parser",
        opts.output_to_console,
        opts.redirect_to_log,
      )
      subtracer.set_output_to_console(false)
      subtracer.set_redirect_to_log(true)
      let parser = @parser.Parser::new(source_code, subtracer)
      let parsed = try {
        parser.parse!()
      } catch {
        Failure(msg) => subtracer.abort(msg)
      }
      self.flags = Flags(@hashset.from_array(parser.get_flags()))
      self.ast = Some(parsed.inner)
    }
    Typecheck => {
      let ast = self.ast.unwrap()
      let subtracer = tracer.new_subtracer(
        "Typer",
        opts.output_to_console,
        opts.redirect_to_log,
      )
      let typer = @typer.Typer::new(ast, subtracer)
      try {
        typer.check_all!()
      } catch {
        Failure(msg) => subtracer.abort(msg)
      }
      self.typechecked = Some(ast)
    }
    Knf => {
      let subtracer = tracer.new_subtracer(
        "Knf",
        opts.output_to_console,
        opts.redirect_to_log,
      )
      let knf_env = @knf.KnfEnv::new(@typer.externals(), subtracer)
      let knf = try {
        let preprocessed = knf_env.syntax_preprocess(
          self.typechecked.unwrap(),
          false,
        )
        knf_env.to_knf!(preprocessed)
      } catch {
        Failure(msg) => subtracer.abort(msg)
      }
      self.knf_env = Some(knf_env)
      self.knf = Some(knf)
    }
    KnfOpt => {
      let _knf_env = self.knf_env.unwrap()
      let tracer = tracer.new_subtracer(
        "KnfOpt",
        opts.output_to_console,
        opts.redirect_to_log,
      )
      let knf = self.knf.unwrap()
      let knf_env = @knf.KnfEnv::new(@typer.externals(), tracer)
      knf_env.init_counter_from_existing(knf)
      let inline = @knf.InlineEnv::new(knf_env, tracer)
      let knf = inline.go(knf, 0)
      let lamlift = @knf.LambdaLiftEnv::new(tracer)
      let knf = lamlift.go(knf)
      self.opt_knf = Some(knf)
    }
    Js => {
      let knf = self.opt_knf.unwrap()
      let tracer = tracer.new_subtracer(
        "Js",
        opts.output_to_console,
        opts.redirect_to_log,
      )
      let lower_ctx = @lower_js.JsLoweringCtx::new(knf, tracer)
      let js = lower_ctx.lower()
      self.js = Some(js)
    }
    Core => {
      let subtracer = tracer.new_subtracer(
        "Core",
        opts.output_to_console,
        opts.redirect_to_log,
      )
      let knf = self.opt_knf.unwrap()
      let core_env = @core.CoreEnv::new(subtracer)
      let core = try {
        core_env.build!(knf)
      } catch {
        Failure(msg) => subtracer.abort(msg)
      }
      self.core_env = Some(core_env)
      self.core = Some(core)
    }
    CoreOpt => {
      let _core_env = self.core_env.unwrap()
      let subtracer = tracer.new_subtracer(
        "CoreOpt",
        opts.output_to_console,
        opts.redirect_to_log,
      )
      let core = self.core.unwrap()
      let opt = @core_opt.CoreOpt::new(core, subtracer)
      opt.go()
      self.core_env = Some(_core_env)
      self.opt_core = Some(core)
    }
    Wasm => {
      let core = self.opt_core.unwrap()
      let subtracer = tracer.new_subtracer(
        "Wasm",
        opts.output_to_console,
        opts.redirect_to_log,
      )
      let lower_ctx = @lower_wasm.WasmLoweringCtx::new(core, subtracer)
      let wasm = lower_ctx.lower()
      self.wasm = Some(wasm)
    }
    Closure => {
      let subtracer = tracer.new_subtracer(
        "Closure",
        opts.output_to_console,
        opts.redirect_to_log,
      )
      let closure_env = @closure.ClosureEnv::new_from_existing(
        self.knf.unwrap(),
        subtracer,
      )
      let closure_ir = try {
        closure_env.knf_to_closure!(
          self.knf.unwrap(),
          Map::from_iter(@typer.externals().iter()),
        )
      } catch {
        Failure(msg) => subtracer.abort(msg)
      }
      self.closure_ir = Some(closure_ir)
    }
    Asm => {
      let core = self.core.unwrap()
      let subtracer = tracer.new_subtracer(
        "Asm",
        opts.output_to_console,
        opts.redirect_to_log,
      )
      let lower_ctx = @lower_riscv.RiscvLoweringCtx::new(core, subtracer)
      let asm = try {
        lower_ctx.lower!()
      } catch {
        Failure(msg) => subtracer.abort(msg)
      }
      self.asm = Some(asm)
    }
    Finished => ()
  }
  self.curr_stage = self.curr_stage.next()
  self.curr_stage >= self.end_stage
}

fn CompileStatus::output(self : CompileStatus, json : Bool) -> String {
  if json {
    match self.curr_stage {
      Parse => self.source_code.unwrap()
      Typecheck => @json.stringify(self.ast.unwrap().to_json())
      Knf => @json.stringify(self.typechecked.unwrap().to_json())
      KnfOpt => @json.stringify(self.knf.unwrap().to_json())
      Js => @json.stringify(self.opt_knf.unwrap().to_json())
      Closure => mt.abort("No json output for JavaScript")
      Core => @json.stringify(self.opt_knf.unwrap().to_json())
      CoreOpt => mt.abort("No json output for core IR")
      Wasm => mt.abort("No json output for wasm")
      Asm => mt.abort("No json output for core IR")
      Finished => Show::to_string(self.asm.unwrap())
    }
  } else {
    match self.curr_stage {
      Parse => self.source_code.unwrap()
      Typecheck => self.ast.unwrap().to_string()
      Knf => self.typechecked.unwrap().to_string()
      KnfOpt => self.knf.unwrap().to_string()
      Js => self.opt_knf.unwrap().to_string()
      Closure => Show::to_string(self.js.unwrap())
      Core => self.closure_ir.unwrap().to_string()
      CoreOpt => Show::to_string(self.core.unwrap().to_pretty_print())
      Wasm => Show::to_string(self.opt_core.unwrap().to_pretty_print())
      Asm => Show::to_string(self.wasm.unwrap())
      Finished => Show::to_string(self.asm.unwrap())
    }
  }
}

struct Options {
  mut json : Bool
  mut js : Bool
  mut wasm : Bool
  mut start_stage : Stages
  mut end_stage : Stages
  mut knf_interpreter : Bool
  mut closure_interpreter : Bool
  mut core_interpreter : Bool
  mut out_file : String
  mut print : Array[String]
  mut file : String?
  mut output_to_console : Bool
  mut redirect_to_log : Bool
}

fn Options::new() -> Options {
  Options::{
    json: false,
    js: false,
    wasm: false,
    start_stage: Stages::Parse,
    end_stage: Stages::Finished,
    knf_interpreter: false,
    closure_interpreter: false,
    core_interpreter: false,
    out_file: "-",
    print: [],
    file: None,
    output_to_console: false,
    redirect_to_log: true,
  }
}

fn Options::update_from_flags(
  _self : Options,
  _flags : Flags,
  _tracer : @util.SubTracer
) -> Unit {

}

fn main {
  let argv = @env.get_args()
  // 
  @ArgParser.parse(
    [
      (
        "--start-stage",
        "-ss",
        @ArgParser.String(
          fn(s) {
            match Stages::from_string(s) {
              Some(stage) => opts.start_stage = stage
              None => mt.abort("Invalid stage")
            }
          },
        ),
        "Start stage",
      ),
      (
        "--end-stage",
        "-es",
        @ArgParser.String(
          fn(s) {
            match Stages::from_string(s) {
              Some(stage) => opts.end_stage = stage
              None => mt.abort("Invalid stage")
            }
          },
        ),
        "End stage",
      ),
      (
        "--no-output-to-console",
        "-notc",
        @ArgParser.Unit(fn() { opts.output_to_console = false }),
        "",
      ),
      (
        "--output-to-console",
        "-otc",
        @ArgParser.Unit(fn() { opts.output_to_console = true }),
        "",
      ),
      (
        "--no-redirect-to-log",
        "-nrtl",
        @ArgParser.Unit(fn() { opts.redirect_to_log = false }),
        "",
      ),
      (
        "--redirect-to-log",
        "-rtl",
        @ArgParser.Unit(fn() { opts.redirect_to_log = true }),
        "",
      ),
      (
        "--knf-interpreter",
        "-ki",
        @ArgParser.Unit(fn() { opts.knf_interpreter = true }),
        "Run with KNF interpreter",
      ),
      (
        "--closure-interpreter",
        "-ci",
        @ArgParser.Unit(fn() { opts.closure_interpreter = true }),
        "Run with closure interpreter",
      ),
      (
        "--core-interpreter",
        "-coi",
        @ArgParser.Unit(fn() { opts.core_interpreter = true }),
        "Run with core interpreter",
      ),
      (
        "--out-file",
        "-o",
        @ArgParser.String(fn(s) { opts.out_file = s }),
        "Output file",
      ),
      (
        "--json",
        "",
        @ArgParser.Unit(fn() { opts.json = true }),
        "Print results as JSON",
      ),
      ("--js", "", @ArgParser.Unit(fn() { opts.js = true }), "Output JS"),
      ("--wasm", "", @ArgParser.Unit(fn() { opts.wasm = true }), "Output WAT"),
      // Regular directives
      (
        "--print",
        "-p",
        @ArgParser.String(fn(s) { opts.print = s.split(",").collect() }),
        "",
      ),
    ],
    fn(s) {
      if opts.file.is_empty().not() {
        mt.abort("multiple files are given")
      }
      opts.file = Some(s)
    },
    "",
    argv,
  )
  mt.set_output_to_console(opts.output_to_console)
  mt.set_redirect_to_log(opts.redirect_to_log)
  // Configure pipeline
  if opts.knf_interpreter {
    opts.end_stage = Stages::Knf
  }
  if opts.closure_interpreter {
    opts.end_stage = Stages::Closure
  }
  if opts.core_interpreter {
    opts.end_stage = Stages::CoreOpt
  }
  if opts.js {
    opts.end_stage = Stages::Js
  }
  if opts.wasm {
    opts.end_stage = Stages::Wasm
  }
  let stages_to_print = opts.print.map(
    fn(s) {
      match Stages::from_string(s) {
        Some(stage) => stage
        None => mt.abort("Invalid stage to print: \{s}")
      }
    },
  )

  // Input
  let file = if opts.file.is_empty() {
    mt.abort("no input file provided")
  } else {
    opts.file.unwrap()
  }
  let contents = @fs.read_to_string(file)

  // Compilation
  let status = match
    CompileStatus::initialize?(
      opts.start_stage,
      opts.end_stage.next(),
      contents,
    ) {
    Ok(status) => status
    Err(e) => mt.abort("Initialization failed: \{Show::to_string(e)}")
  }

  // Run stages
  while true {
    let stg = status.curr_stage
    let stop = status.step()
    let flags = status.flags
    opts.update_from_flags(flags, mt)
    if stages_to_print.contains(stg) {
      mt.println("Stage: \{stg}")
      mt.println(status.output(opts.json))
    }
    if stop {
      break
    }
  }

  // Output
  if opts.knf_interpreter {
    let knfi = @knf_eval.KnfInterpreter::new()
    add_interpreter_fns(knfi)
    match knfi.eval_full?(status.knf.unwrap()) {
      Ok(_) => ()
      Err(Failure(e)) => {
        mt.println(e)
        mt.abort("KNF interpreter error")
      }
    }
  } else if opts.closure_interpreter {
    let clsi = @closure_eval.ClosureInterpreter::new()
    add_closure_interpreter_fns(clsi)
    match clsi.eval_full?(status.closure_ir.unwrap()) {
      Ok(_) => ()
      Err(Failure(e)) => {
        mt.println(e)
        mt.abort("Closure interpreter error")
      }
    }
  } else if opts.core_interpreter {
    let corei = @core_eval.CoreInterpreter::new(mt)
    match corei.interpret?(status.core.unwrap()) {
      Ok(_) => ()
      Err(Failure(e)) => {
        mt.println(e)
        mt.abort("Core interpreter error")
      }
    }
  } else {
    let out_string = status.output(opts.json)
    mt.println(out_string)
    if false &&
      (
        opts.out_file.contains("speed/matmul-flat-big") ||
        opts.out_file.contains("speed/iters")
      ) {
      mt.abort(
        "we skip it because it seems to affect the performance of other tests",
      )
    }
    if opts.out_file != "-" {
      @fs.write_to_string(opts.out_file, out_string)
    }
  }
  let file_name = opts.file.unwrap().split(".").collect()
  let file_name = file_name[0:file_name.length() - 1]
  let file_name = file_name.iter().collect().join("")
  let log_buf = Buffer::new()
  tracer.done(
    fn(_name, log) {
      log_buf.write_string(log)
      log_buf.write_string("\n")
    },
    fn(name, record) { @fs.write_to_string("\{file_name}.\{name}.txt", record) },
  )
  @fs.write_to_string("\{file_name}.log", log_buf.to_string())
}

fn add_interpreter_fns(interpreter : @knf_eval.KnfInterpreter) -> Unit {
  interpreter.add_extern_fn(
    "minimbt_print_int",
    fn(args) {
      match args[0] {
        Int(i) => ot.print(i)
        _ => mt.abort("print_int expects Int")
      }
      Unit
    },
  )
  interpreter.add_extern_fn(
    "minimbt_print_endline",
    fn(_args) {
      ot.print("\n")
      Unit
    },
  )
  let create_array = fn(args : Array[@knf_eval.Value]) {
    match args[0] {
      Int(n) => @knf_eval.Value::Array(Array::make(n, args[1]))
      _ => ot.abort("create_array expects Int")
    }
  }
  interpreter.add_extern_fn("minimbt_create_array", create_array)
  interpreter.add_extern_fn("minimbt_create_float_array", create_array)
  interpreter.add_extern_fn("minimbt_create_ptr_array", create_array)
  interpreter.add_extern_fn(
    "minimbt_truncate",
    fn(args) {
      match args[0] {
        Double(d) => Int(d.to_int())
        _ => mt.abort("truncate expects Double")
      }
    },
  )
  interpreter.add_extern_fn(
    "minimbt_sin",
    fn(args) {
      match args[0] {
        Double(d) => Double(@math.sin(d))
        _ => mt.abort("sin expects Double")
      }
    },
  )
  interpreter.add_extern_fn(
    "minimbt_cos",
    fn(args) {
      match args[0] {
        Double(d) => Double(@math.cos(d))
        _ => mt.abort("cos expects Double")
      }
    },
  )
  interpreter.add_extern_fn(
    "minimbt_sqrt",
    fn(args) {
      match args[0] {
        Double(d) => Double(d.sqrt())
        _ => mt.abort("sqrt expects Double")
      }
    },
  )
  interpreter.add_extern_fn(
    "minimbt_abs_float",
    fn(args) {
      match args[0] {
        Double(d) => Double(@double.abs(d))
        _ => mt.abort("abs_float expects Double")
      }
    },
  )
  interpreter.add_extern_fn(
    "minimbt_int_of_float",
    fn(args) {
      match args[0] {
        Double(d) => Int(d.to_int())
        _ => mt.abort("int_of_float expects Double")
      }
    },
  )
  interpreter.add_extern_fn(
    "minimbt_float_of_int",
    fn(args) {
      match args[0] {
        Int(i) => Double(i.to_double())
        _ => mt.abort("float_of_int expects Int")
      }
    },
  )
}

fn add_closure_interpreter_fns(
  interpreter : @closure_eval.ClosureInterpreter
) -> Unit {
  interpreter.add_extern_fn(
    "minimbt_print_int",
    fn(args) {
      match args[0] {
        Int(i) => ot.print(i)
        _ => mt.abort("print_int expects Int")
      }
      Unit
    },
  )
  interpreter.add_extern_fn(
    "minimbt_print_endline",
    fn(_args) {
      ot.print("\n")
      Unit
    },
  )
  let create_array = fn(args : Array[@closure_eval.Value]) {
    match args[0] {
      Int(n) => @closure_eval.Value::Array(Array::make(n, args[1]))
      _ => mt.abort("create_array expects Int")
    }
  }
  interpreter.add_extern_fn("minimbt_create_array", create_array)
  interpreter.add_extern_fn("minimbt_create_float_array", create_array)
  interpreter.add_extern_fn("minimbt_create_ptr_array", create_array)
  interpreter.add_extern_fn(
    "minimbt_truncate",
    fn(args) {
      match args[0] {
        Double(d) => Int(d.to_int())
        _ => mt.abort("truncate expects Double")
      }
    },
  )
  interpreter.add_extern_fn(
    "minimbt_sin",
    fn(args) {
      match args[0] {
        Double(d) => Double(@math.sin(d))
        _ => mt.abort("sin expects Double")
      }
    },
  )
  interpreter.add_extern_fn(
    "minimbt_cos",
    fn(args) {
      match args[0] {
        Double(d) => Double(@math.cos(d))
        _ => mt.abort("cos expects Double")
      }
    },
  )
  interpreter.add_extern_fn(
    "minimbt_sqrt",
    fn(args) {
      match args[0] {
        Double(d) => Double(d.sqrt())
        _ => mt.abort("sqrt expects Double")
      }
    },
  )
  interpreter.add_extern_fn(
    "minimbt_abs_float",
    fn(args) {
      match args[0] {
        Double(d) => Double(@double.abs(d))
        _ => mt.abort("abs_float expects Double")
      }
    },
  )
  interpreter.add_extern_fn(
    "minimbt_int_of_float",
    fn(args) {
      match args[0] {
        Double(d) => Int(d.to_int())
        _ => mt.abort("int_of_float expects Double")
      }
    },
  )
  interpreter.add_extern_fn(
    "minimbt_float_of_int",
    fn(args) {
      match args[0] {
        Int(i) => Double(i.to_double())
        _ => mt.abort("float_of_int expects Int")
      }
    },
  )
}

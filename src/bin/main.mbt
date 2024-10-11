/// The stage to be run next.
enum Stages {
  Parse
  Typecheck
  Knf
  KnfOpt
  Closure
  Core
  Asm
  Finished
} derive(Show, Eq, Compare)

fn Stages::from_string(s : String) -> Stages? {
  match s {
    "parse" => Some(Stages::Parse)
    "typecheck" => Some(Stages::Typecheck)
    "knf" => Some(Stages::Knf)
    "knf-opt" => Some(Stages::KnfOpt)
    "core" => Some(Stages::Core)
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
    Stages::KnfOpt => Stages::Closure
    Stages::Closure => Stages::Core
    Stages::Core => Stages::Asm
    Stages::Asm => Stages::Finished
    Stages::Finished => Stages::Finished
  }
}

struct CompileStatus {
  mut curr_stage : Stages
  end_stage : Stages
  mut source_code : String?
  mut ast : @types.Syntax?
  mut typechecked : @types.Syntax?
  knf_env : @knf.KnfEnv
  mut knf : @knf.Knf?
  mut opt_knf : @knf.Knf?
  mut opt : @core.Core?
  mut closure_ir : @closure.Program?
  mut asm : @lower_riscv.MCore?
}

fn CompileStatus::initialize(
  start_stage : Stages,
  end_stage : Stages,
  file : String
) -> CompileStatus! {
  let v = CompileStatus::{
    curr_stage: start_stage,
    end_stage,
    source_code: None,
    ast: None,
    typechecked: None,
    knf_env: @knf.KnfEnv::new(@typer.externals()),
    knf: None,
    opt_knf: None,
    closure_ir: None,
    asm: None,
    opt: None,
  }
  match start_stage {
    Parse => v.source_code = Some(file)
    Typecheck => v.ast = Some(@types.Syntax::from_json!(@json.parse!(file)))
    Knf => v.typechecked = Some(@types.Syntax::from_json!(@json.parse!(file)))
    KnfOpt => {
      v.knf = Some(@knf.Knf::from_json!(@json.parse!(file)))
      v.knf_env.init_counter_from_existing(v.knf.unwrap())
    }
    Closure => {
      v.opt_knf = Some(@knf.Knf::from_json!(@json.parse!(file)))
      v.knf_env.init_counter_from_existing(v.opt_knf.unwrap())
    }
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
      let parser = @parser.Parser::new(source_code)
      let parsed = try {
        parser.parse!()
      } catch {
        Failure(msg) => {
          println(msg)
          @util.die("Parse error")
        }
      }
      self.ast = Some(parsed.inner)
    }
    Typecheck => {
      let ast = self.ast.unwrap()
      let typer = @typer.Typer::new(ast)
      try {
        typer.check_all!()
      } catch {
        Failure(msg) => {
          println(msg)
          @util.die("Typecheck error")
        }
      }
      self.typechecked = Some(ast)
    }
    Knf => {
      let knf = try {
        let preprocessed = self.knf_env.syntax_preprocess(
          self.typechecked.unwrap(),
          false,
          false,
        )
        self.knf_env.to_knf!(preprocessed)
      } catch {
        Failure(msg) => {
          println(msg)
          @util.die("Knf error")
        }
      }
      self.knf = Some(knf)
    }
    KnfOpt => {
      let knf = self.knf.unwrap()
      // println("\{Show::to_string(knf.to_pretty_print())}")
      let lamlift = @knf.LambdaLiftEnv::new()
      let knf = lamlift.go(knf)
      // println("\n\n\nOpt: \{Show::to_string(knf.to_pretty_print())}")
      self.opt_knf = Some(knf)
    }
    Core => {
      let knf = self.opt_knf.unwrap()
      let core_env = @core.CoreEnv::new()
      let core = try {
        core_env.build!(knf)
      } catch {
        Failure(msg) => {
          println(msg)
          @util.die("Core error")
        }
      }
      // println("\{Show::to_string(core.to_pretty_print())}")
      let opt = @core_opt.CoreOpt::new(core)
      opt.go()
      self.opt = Some(core)
    }
    Closure => {
      let closure_env = @closure.ClosureEnv::new_from_existing(
        self.knf.unwrap(),
      )
      let closure_ir = try {
        closure_env.knf_to_closure!(
          self.knf.unwrap(),
          Map::from_iter(@typer.externals().iter()),
        )
      } catch {
        Failure(msg) => {
          println(msg)
          @util.die("Closure error")
        }
      }
      self.closure_ir = Some(closure_ir)
    }
    Asm => {
      let core = self.opt.unwrap()
      let lower_ctx = @lower_riscv.RiscvLoweringCtx::new(core)
      let asm = try {
        lower_ctx.lower!()
      } catch {
        Failure(msg) => {
          println(msg)
          @util.die("Asm error")
        }
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
      Closure => @json.stringify(self.opt_knf.unwrap().to_json())
      Core => @json.stringify(self.closure_ir.unwrap().to_json())
      Asm => @util.die("No json output for core IR")
      Finished => Show::to_string(self.asm.unwrap())
    }
  } else {
    match self.curr_stage {
      Parse => self.source_code.unwrap()
      Typecheck => self.ast.unwrap().to_string()
      Knf => self.typechecked.unwrap().to_string()
      KnfOpt => self.knf.unwrap().to_string()
      Closure => self.opt_knf.unwrap().to_string()
      Core => self.closure_ir.unwrap().to_string()
      Asm => Show::to_string(self.opt.unwrap().to_pretty_print())
      Finished => Show::to_string(self.asm.unwrap())
    }
  }
}

fn main {
  let argv = @env.get_args()
  let mut file = None
  let knf_opt_iters = Ref::new(10)
  let knf_opt_inline_threshold = Ref::new(10)

  // Testing directives
  let json = Ref::new(false)
  let start_stage = Ref::new(Stages::Parse)
  let end_stage = Ref::new(Stages::Finished)
  let knf_interpreter = Ref::new(false)
  let closure_interpreter = Ref::new(false)
  let core_interpreter = Ref::new(false)
  let out_file = Ref::new("-")
  let print = Ref::new([])

  // 
  @ArgParser.parse(
    [
      (
        "--start-stage",
        "",
        @ArgParser.String(
          fn(s) {
            match Stages::from_string(s) {
              Some(stage) => start_stage.val = stage
              None => @util.die("Invalid stage")
            }
          },
        ),
        "Start stage",
      ),
      (
        "--end-stage",
        "",
        @ArgParser.String(
          fn(s) {
            match Stages::from_string(s) {
              Some(stage) => end_stage.val = stage
              None => @util.die("Invalid stage")
            }
          },
        ),
        "End stage",
      ),
      (
        "--knf-interpreter",
        "",
        @ArgParser.Set(knf_interpreter),
        "Run with KNF interpreter",
      ),
      (
        "--closure-interpreter",
        "",
        @ArgParser.Set(closure_interpreter),
        "Run with closure interpreter",
      ),
      (
        "--core-interpreter",
        "",
        @ArgParser.Set(core_interpreter),
        "Run with core interpreter",
      ),
      (
        "--out-file",
        "-o",
        @ArgParser.String(fn(s) { out_file.val = s }),
        "Output file",
      ),
      ("--json", "", @ArgParser.Set(json), "Print results as JSON"),

      // Regular directives
      (
        "--print",
        "",
        @ArgParser.String(fn(s) { print.val = s.split(",").collect() }),
        "",
      ),
      (
        "--knf-opt-iters",
        "N",
        @ArgParser.String(
          fn(s) {
            let i = @strconv.parse_int?(s)
            match i {
              Ok(i) => knf_opt_iters.val = i
              Err(_) => @util.die("Invalid number")
            }
          },
        ),
        "Number of optimization iterations",
      ),
      (
        "--knf-opt-inline-threshold",
        "N",
        @ArgParser.String(
          fn(s) {
            let i = @strconv.parse_int?(s)
            match i {
              Ok(i) => knf_opt_inline_threshold.val = i
              Err(_) => @util.die("Invalid number")
            }
          },
        ),
        "Inline threshold for KNF optimization",
      ),
    ],
    fn(s) {
      if file.is_empty().not() {
        @util.die("multiple files are given")
      }
      file = Some(s)
    },
    "",
    argv,
  )

  // Configure pipeline
  if knf_interpreter.val {
    end_stage.val = Stages::Knf
  }
  if closure_interpreter.val {
    end_stage.val = Stages::Closure
  }
  if core_interpreter.val {
    end_stage.val = Stages::Core
  }
  let stages_to_print = print.val.map(
    fn(s) {
      match Stages::from_string(s) {
        Some(stage) => stage
        None => @util.die("Invalid stage to print: \{s}")
      }
    },
  )

  // Input
  let file = if file.is_empty() {
    @util.die("no input file provided")
  } else {
    file.unwrap()
  }
  let contents = @fs.read_to_string(file)

  // Compilation
  let status = match
    CompileStatus::initialize?(start_stage.val, end_stage.val.next(), contents) {
    Ok(status) => status
    Err(e) => {
      println(e)
      @util.die("Initialization error")
    }
  }

  // Run stages
  while true {
    let stg = status.curr_stage
    let stop = status.step()
    if stages_to_print.contains(stg) {
      println("Stage: \{stg}")
      println(status.output(json.val))
    }
    if stop {
      break
    }
  }

  // Output
  if knf_interpreter.val {
    let knfi = @knf_eval.KnfInterpreter::new()
    add_interpreter_fns(knfi)
    match knfi.eval_full?(status.knf.unwrap()) {
      Ok(_) => ()
      Err(Failure(e)) => {
        println(e)
        @util.die("KNF interpreter error")
      }
    }
  } else if closure_interpreter.val {
    let clsi = @closure_eval.ClosureInterpreter::new()
    add_closure_interpreter_fns(clsi)
    match clsi.eval_full?(status.closure_ir.unwrap()) {
      Ok(_) => ()
      Err(Failure(e)) => {
        println(e)
        @util.die("Closure interpreter error")
      }
    }
  } else if core_interpreter.val {
    let corei = @core_eval.CoreInterpreter::new()
    match corei.interpret?(status.opt.unwrap()) {
      Ok(_) => ()
      Err(Failure(e)) => {
        println(e)
        @util.die("Core interpreter error")
      }
    }
  } else {
    let out_string = status.output(json.val)
    if out_file.val == "-" {
      println(out_string)
    } else {
      @fs.write_to_string(out_file.val, out_string)
    }
  }
}

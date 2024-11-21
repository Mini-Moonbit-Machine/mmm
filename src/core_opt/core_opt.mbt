pub struct CoreOpt {
  core : @core.Core
  tracer : @util.SubTracer
  conf : @types.Configure
}

pub fn CoreOpt::new(
  core : @core.Core,
  conf : @types.Configure,
  tracer : @util.SubTracer
) -> CoreOpt {
  { core, conf, tracer }
}

fn CoreOpt::dfe(self : CoreOpt, id : Int) -> Unit {
  let core = self.core
  let calls = @core_analysis.CallAnalysis::new(core, self.tracer)
  let fcf = calls.compute()
  let old_funcs = core.funcs.iter().collect()
  for func in old_funcs {
    let (fref, func) = func
    if fcf._.get(fref).unwrap() == @core_analysis.CallFrequncy::Never {
      self.tracer.println("Remove dead function: \{fref}")
      core.funcs.remove(fref)
      core.names.remove(func.name)
    }
  }
  self.tracer.record("core_opt_after_dfe_\{id}", core.to_pretty_print())
}

pub fn CoreOpt::go(self : CoreOpt) -> Unit {
  let core = self.core
  let o2f = Offset2Field::new(core)
  o2f.go()
  self.tracer.record("core_opt_after_o2f", core.to_pretty_print())
  let tre = TailRecElim::new(core, self.tracer)
  tre.go()
  self.tracer.record("core_opt_after_tre", core.to_pretty_print())
  if self.conf.core_licm {
    let licm = LICM::new(core, self.tracer)
    licm.go()
  }
  self.tracer.record("core_opt_after_licm", core.to_pretty_print())
  for i in 0..<self.conf.core_inline {
    let calls = @core_analysis.CallAnalysis::new(core, self.tracer)
    let fcf = calls.compute()
    self.tracer.println("Call frequnce: \{fcf}")
    let inliner = Inliner::new(core, fcf, self.tracer)
    inliner.go(i)
    self.dfe(i)
  }
  if self.conf.core_lvn {
    let lvn = LocalValueNumbering::new(core, self.tracer)
    lvn.go()
  }
  self.tracer.record("core_opt_after_lvn", core.to_pretty_print())
}

// WARNING: CoreOpt2 assumes that it targets to riscv64.
pub struct CoreOpt2 {
  core : @core.Core
  conf : @types.Configure
  tracer : @util.SubTracer
}

pub fn CoreOpt2::new(
  core : @core.Core,
  conf : @types.Configure,
  tracer : @util.SubTracer
) -> CoreOpt2 {
  { core, conf, tracer }
}

pub fn CoreOpt2::go(self : CoreOpt2) -> Unit {
  let core = self.core
  if self.conf.core_2_fo {
    let float_offset = FloatOffset::new(core, self.tracer)
    float_offset.go()
  }
  self.tracer.record("core_opt2_after_fo", core.to_pretty_print())
  if self.conf.core_2_lvn {
    let lvn = LocalValueNumbering::new(core, self.tracer)
    lvn.go()
  }
  self.tracer.record("core_opt2_after_lvn", core.to_pretty_print())
  if self.conf.core_2_jtbl {
    let jtbl = JumpTable::new(core, self.tracer)
    jtbl.go()
  }
  self.tracer.record("core_opt2_after_jtbl", core.to_pretty_print())
}

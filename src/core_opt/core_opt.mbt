pub struct CoreOpt {
  core : @core.Core
  tracer : @util.SubTracer
}

pub fn CoreOpt::new(core : @core.Core, tracer : @util.SubTracer) -> CoreOpt {
  { core, tracer }
}

pub fn CoreOpt::go(self : CoreOpt) -> Unit {
  let core = self.core
  let o2f = Offset2Field::new(core)
  o2f.go()
  self.tracer.record("core_opt_after_o2f", core.to_pretty_print())
  let tre = TailRecElim::new(core, self.tracer)
  tre.go()
  self.tracer.record("core_opt_after_tre", core.to_pretty_print())
  let calls = @core_analysis.CallAnalysis::new(core, self.tracer)
  let fcf = calls.compute()
  self.tracer.println("Call frequnce: \{fcf}")
  let inliner = Inliner::new(core, fcf, self.tracer)
  inliner.go()
  self.tracer.record("core_opt_after_inline", core.to_pretty_print())
}

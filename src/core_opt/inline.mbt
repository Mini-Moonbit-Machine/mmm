pub struct Inliner {
  core : @core.Core
  tracer : @util.SubTracer
}

pub fn Inliner::new(core : @core.Core, tracer : @util.SubTracer) -> Inliner {
  { core, tracer }
}

pub struct CoreOpt {
  core : @core.Core
}

pub fn CoreOpt::new(core : @core.Core) -> CoreOpt {
  { core, }
}

pub fn CoreOpt::go(self : CoreOpt) -> Unit {
  let core = self.core
  let o2f = Offset2Field::new(core)
  o2f.go()
}

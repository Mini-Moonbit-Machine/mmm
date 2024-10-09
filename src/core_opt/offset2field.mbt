pub struct Offset2Field {
  core : @core.Core
}

pub fn Offset2Field::new(core : @core.Core) -> Offset2Field {
  { core, }
}

pub fn Offset2Field::go(self : Offset2Field) -> Unit {
  fn offset2field(addr : @core.Address) -> @core.Address {
    match addr {
      Offset(~ty, ~val, offset=@core.Value::Int32(val=imm)) =>
        @core.Address::Field(~ty, ~val, index=imm)
      _ => addr
    }
  }

  for func in self.core.funcs {
    let (_, func) = func
    let updated = []
    for insn in func.insns {
      let (iref, insn) = insn
      match insn {
        Store(~ty, ~ma, ~val) => {
          let new_insn = @core.Insn::Store(~ty, ma=offset2field(ma), ~val)
          updated.push((iref, new_insn))
        }
        Load(~ty, ~ma) => {
          let new_insn = @core.Insn::Load(~ty, ma=offset2field(ma))
          updated.push((iref, new_insn))
        }
        _ => ()
      }
    }
    for ipair in updated {
      let (iref, insn) = ipair
      func.insns[iref] = insn
    }
  }
}

pub struct InsnSel {
  mut local_virt_regs_counter : Int
}

pub fn InsnSel::new() -> InsnSel {
  { local_virt_regs_counter: 0 }
}

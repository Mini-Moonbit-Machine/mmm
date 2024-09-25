pub type MFnRef @core.Id derive(Eq, Hash)

typealias MutMap[K, V] = @hashmap.T[K, V]

pub struct MCore {
  funcs : MutMap[Label, MFn]
  globals : MutMap[Label, Unit]
}

pub struct MFn {
  id : MFnRef
  name : String
  entry : Label
  blocks : MutMap[Label, MBasicBlock]
}

pub enum MParam {
  Reg(Reg)
  FReg(FReg)
}

pub struct MBasicBlock {
  id : Label
  params : Array[MParam]
  insns : Array[RvAsm]
  control : Array[RvAsm]
}

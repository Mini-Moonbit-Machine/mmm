extern "wasm" fn ffi(n : Int) -> Int =
  #| (func i32.const 0 drop)

pub fn exit(n : Int) -> Unit {
  let _ = ffi(n)

}

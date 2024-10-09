let limit: Double = 4.0;
let niter: Int = 50;
fn mandelbrot(w: Int) -> Unit {
  let h = w;
  let fw = float_of_int(w) / 2.0;
  let fh = float_of_int(h) / 2.0;
  let red_h = h - 1;
  let red_w = w - 1;
  let byte = Array::make(1, 0);
  fn loop1(y: Int) {
    if y <= red_h {
      let ci = float_of_int(y) / fh - 1.0;
      fn loop2(x: Int) {
        if x <= red_w {
          let cr = float_of_int(x) / fw - 1.5;
          let zr = Array::make(1, 0.0);
          let zi = Array::make(1, 0.0);
          let trm = Array::make(1, 0.0);
          let n = Array::make(1, 0);
          fn loop3() {
            zi[0] = 2.0 * zr[0] * zi[0] + ci;
            zr[0] = trm[0] + cr;
            let tr = zr[0] * zr[0];
            let ti = zi[0] * zi[0];
            if limit <= tr + ti {
              byte[0] = byte[0] * 2;
              ()
            } else {
              n[0] = n[0] + 1;
              if n[0] == niter {
                byte[0] = byte[0] * 2 + 1;
                ()
              } else {
                trm[0] = tr - ti;
                loop3()
              }
            }
          };
          loop3();
          if (x + 1) / 8 * 8 == x + 1 {
            print_int(byte[0]);
            print_endline();
            ()
          };
          loop2(x + 1)
        } else {
          ()
        }
      };
      loop2(0);
      loop1(y + 1)
    } else {
      ()
    }
  };
  loop1(0)
};

fn main {
  mandelbrot(320);
  ()
}
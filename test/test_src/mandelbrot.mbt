
fn mandelbrot(w: Int) -> Unit {
  fn dbl(f: Double) -> Double {
    f + f
  };
  fn yloop(y: Int) -> Unit {
    if w <= y {
      ()
    } else {
      fn xloop(x: Int, y: Int) -> Unit {
        if w <= x {
          print_endline();
          ()
        } else {
          let cr = dbl(float_of_int(x)) / float_of_int(w) - 1.5;
          let ci = dbl(float_of_int(y)) / float_of_int(w) - 1.0;
          fn iloop(i: Int, zr: Double, zi: Double, zr2: Double, zi2: Double, cr: Double, ci: Double) -> Unit {
            if i == 0 {
              print_int(1)
            } else {
              let tr = zr2 - zi2 + cr;
              let ti = dbl(zr) * zi + ci;
              let zr = tr;
              let zi = ti;
              let zr2 = zr * zr;
              let zi2 = zi * zi;
              if 2.0 * 2.0 <= zr2 + zi2 {
                print_int(0)
              } else {
                iloop(i - 1, zr, zi, zr2, zi2, cr, ci)
              }
            }
          };
          iloop(1000, 0.0, 0.0, 0.0, 0.0, cr, ci);
          xloop(x + 1, y)
        }
      };
      xloop(0, y);
      yloop(y + 1)
    }
  };
  yloop(0)
};

fn main {
  mandelbrot(1000)
}

fn fma_int(
  vi11: Int,
  vi12: Int,
  vi13: Int,
  vi14: Int,
  vi21: Int,
  vi22: Int,
  vi23: Int,
  vi24: Int,
  vi31: Int,
  vi32: Int,
  vi33: Int,
  vi34: Int
) -> (Int, Int, Int, Int) {
  let vir1 = vi11 * vi21 + vi31;
  let vir2 = vi12 * vi22 + vi32;
  let vir3 = vi13 * vi23 + vi33;
  let vir4 = vi14 * vi24 + vi34;
  (vir1, vir2, vir3, vir4)
};

fn fma_double(
  vd11: Double,
  vd12: Double,
  vd13: Double,
  vd14: Double,
  vd21: Double,
  vd22: Double,
  vd23: Double,
  vd24: Double,
  vd31: Double,
  vd32: Double,
  vd33: Double,
  vd34: Double
) -> (Double, Double, Double, Double) {
  let vdr1 = vd11 * vd21 + vd31;
  let vdr2 = vd12 * vd22 + vd32;
  let vdr3 = vd13 * vd23 + vd33;
  let vdr4 = vd14 * vd24 + vd34;
  (vdr1, vdr2, vdr3, vdr4)
};


fn fma_mixed(
  vi11: Int,
  vi12: Int,
  vi13: Int,
  vi14: Int,
  vi21: Int,
  vi22: Int,
  vi23: Int,
  vi24: Int,
  vi31: Int,
  vi32: Int,
  vi33: Int,
  vi34: Int,
  vd11: Double,
  vd12: Double,
  vd13: Double,
  vd14: Double,
  vd21: Double,
  vd22: Double,
  vd23: Double,
  vd24: Double,
  vd31: Double,
  vd32: Double,
  vd33: Double,
  vd34: Double
) -> ((Int, Int, Int, Int), (Double, Double, Double, Double)) {
  let vir = fma_int(vi11, vi12, vi13, vi14, vi21, vi22, vi23, vi24, vi31, vi32, vi33, vi34);
  let vdr = fma_double(vd11, vd12, vd13, vd14, vd21, vd22, vd23, vd24, vd31, vd32, vd33, vd34);
  (vir, vdr)
};


fn main {
  let vi_arr = Array::make(3, (0, 0, 0, 0));
  fn init_vi_arr(i: Int) {
    if i <= 2 {
      vi_arr[i] = (i, i + 1, i + 2, i + 3);
      init_vi_arr(i + 1);
    } else {
      ()
    };
  };
  init_vi_arr(0);
  let vd_arr = Array::make(3, (0.0, 0.0, 0.0, 0.0));
  fn init_vd_arr(i: Int) {
    if i <= 2 {
      vd_arr[i] = (float_of_int(i), float_of_int(i + 1), float_of_int(i + 2), float_of_int(i + 3));
      init_vd_arr(i + 1);
    } else {
      ()
    };
  };
  init_vd_arr(0);


  fn iter_fma() {
    let (vi11, vi12, vi13, vi14) = vi_arr[0];
    let (vi21, vi22, vi23, vi24) = vi_arr[1];
    let (vi31, vi32, vi33, vi34) = vi_arr[2];
    let (vd11, vd12, vd13, vd14) = vd_arr[0];
    let (vd21, vd22, vd23, vd24) = vd_arr[1];
    let (vd31, vd32, vd33, vd34) = vd_arr[2];
    let (vir, vdr) = fma_mixed(vi11, vi12, vi13, vi14, vi21, vi22, vi23, vi24, vi31, vi32, vi33, vi34, vd11, vd12, vd13, vd14, vd21, vd22, vd23, vd24, vd31, vd32, vd33, vd34);

    let (vi11, vi12, vi13, vi14) = vir;
    let (vd11, vd12, vd13, vd14) = vdr;
    let (vir, vdr) = fma_mixed(vi11, vi12, vi13, vi14, vi21, vi22, vi23, vi24, vi31, vi32, vi33, vi34, vd11, vd12, vd13, vd14, vd21, vd22, vd23, vd24, vd31, vd32, vd33, vd34);

    let (vi21, vi22, vi23, vi24) = vir;
    let (vd21, vd22, vd23, vd24) = vdr;
    let (vir, vdr) = fma_mixed(vi11, vi12, vi13, vi14, vi21, vi22, vi23, vi24, vi31, vi32, vi33, vi34, vd11, vd12, vd13, vd14, vd21, vd22, vd23, vd24, vd31, vd32, vd33, vd34);

    let (vi31, vi32, vi33, vi34) = vir;
    let (vd31, vd32, vd33, vd34) = vdr;
    let (vir, vdr) = fma_mixed(vi11, vi12, vi13, vi14, vi21, vi22, vi23, vi24, vi31, vi32, vi33, vi34, vd11, vd12, vd13, vd14, vd21, vd22, vd23, vd24, vd31, vd32, vd33, vd34);

    vi_arr[0] = vir;
    vd_arr[0] = vdr;
  };

  fn stress(i: Int) {
    if i <= 3 {
      iter_fma();
      stress(i + 1);
    } else {
      ()
    };
  };
  stress(1);
  let (vi11, vi12, vi13, vi14) = vi_arr[0];
  let (vd11, vd12, vd13, vd14) = vd_arr[0];
  print_int(vi11);
  print_char(10);
  print_int(int_of_float(vd11));
};
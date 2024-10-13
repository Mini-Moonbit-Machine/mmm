fn fannkuch(n: Int) -> (Int, Int) {
  fn gen_perm1(len: Int) -> Array[Int] {
    let arr = Array::make(len, 0);
    fn init_arr(i: Int) -> Unit {
      if i <= len - 1 {
        arr[i] = i;
        init_arr(i + 1)
      } else {
        ()
      }
    };
    let _ = init_arr(0);
    arr
  };
  let perm1 = gen_perm1(n);
  let perm = Array::make(n, 0);
  let count = Array::make(n, 0);
  let flips = Array::make(1, 0);
  let max_flips = Array::make(1, 0);
  let check_sum = Array::make(1, 0);
  let nperm = Array::make(1, 0);
  let r = Array::make(1, n);
  fn compute(r: Array[Int]) -> Unit {
    if 1 <= r[0] {
      fn copy_perm(i: Int) -> Unit {
        if i <= n - 1 {
          perm[i] = perm1[i];
          copy_perm(i + 1)
        } else {
          ()
        }
      };
      let _ = copy_perm(0);

      fn init_count() -> Unit {
        if not(r[0] == 1) {
          count[r[0] - 1] = r[0];
          r[0] = r[0] - 1;
          init_count()
        };
      };
      let _ = init_count();

      flips[0] = 0;
      let k = Array::make(1, perm[0]);
      fn do_flip(k: Array[Int]) -> Unit { // remove k
        if not(k[0] == 0) {
          let t = Array::make(1, 0);
          fn flip_loop(i: Int) -> Unit {
            if i <= k[0] / 2 {
              t[0] = perm[i];
              perm[i] = perm[k[0] - i];
              perm[k[0] - i] = t[0];
              flip_loop(i + 1)
            } else {
              ()
            }
          };
          let _ = flip_loop(0);
          k[0] = perm[0];
          flips[0] = flips[0] + 1;
          do_flip(k)
        }
      };
      let _ = do_flip(k);

      if max_flips[0] <= flips[0] {
        max_flips[0] = flips[0]
      };
      check_sum[0] = check_sum[0] + flips[0] * (1 - (nperm[0] - (nperm[0] / 2 * 2)) * 2);

      let go = Array::make(1, true);
      let t = Array::make(1, 0);
      fn go_loop() -> Unit {
        if go[0] {
          if r[0] == n {
            go[0] = false;
            r[0] = 0;
            ()
          } else {
            t[0] = perm1[0];
            fn move_perm1_forward(i: Int) -> Unit {
              if i <= r[0] - 1 {
                perm1[i] = perm1[i + 1];
                let _ = move_perm1_forward(i + 1);
                ()
              } else {
                ()
              }
            };
            let _ = move_perm1_forward(0);

            perm1[r[0]] = t[0];

            count[r[0]] = count[r[0]] - 1;
            if 1 <= count[r[0]] {
              go[0] = false
            } else {
              r[0] = r[0] + 1
            };
            ()
          };
          go_loop()
        } else {
          ()
        }
      };
      let _ = go_loop();

      nperm[0] = nperm[0] + 1;
      compute(r)
    } else {
      ()
    }
  };
  let _ = compute(r);

  (max_flips[0], check_sum[0])
};

fn main {
  let (max_flips, check_sum) = fannkuch(9);
  print_int(check_sum);
  print_int(max_flips)
};
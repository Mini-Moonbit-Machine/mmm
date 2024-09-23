fn bf(len : Int, prog : Array[Int]) -> Unit {
  let ptr = Array::make(1, 0);
  let cells = Array::make(1024, 0);
  fn aux(pc) {
    if len <= pc {
      pc
    } else {
      let i = prog[pc];
      if i == 1 { // '>'
        ptr[0] = ptr[0] + 1;
        aux(pc + 1)
      } else if i == 2 { // '<'
        ptr[0] = ptr[0] - 1;
        aux(pc + 1)
      } else if i == 3 { // '+'
        cells[ptr[0]] = cells[ptr[0]] + 1;
        aux(pc + 1)
      } else if i == 4 { // '-'
        cells[ptr[0]] = cells[ptr[0]] - 1;
        aux(pc + 1)
      } else if i == 5 { // '.'
        print_char(cells[ptr[0]]);
        aux(pc + 1)
      } else if i == 7 { // '['
        let next_pc = Array::make(1, pc);
        fn lp() {
          if cells[ptr[0]] == 0 {
            ()
          } else {
            next_pc[0] = aux(pc + 1);
            lp()
          }
        };
        lp();
        aux(next_pc[0] + 1)
      } else if i == 8 { // ']'
        pc
      } else {
        pc
      }
    }
  };
  let _ = aux(0);
  ()
};

fn main {
  let prog = Array::make(23, 0);
  prog[0] = 3;
  prog[1] = 3;
  prog[2] = 3;
  prog[3] = 3;
  prog[4] = 3;
  prog[5] = 3;
  prog[6] = 3;
  prog[7] = 3;
  prog[8] = 7;
  prog[9] = 1;
  prog[10] = 3;
  prog[11] = 3;
  prog[12] = 3;
  prog[13] = 3;
  prog[14] = 3;
  prog[15] = 3;
  prog[16] = 3;
  prog[17] = 3;
  prog[18] = 2;
  prog[19] = 4;
  prog[20] = 8;
  prog[21] = 1;
  prog[22] = 5;
  bf(23, prog)
};
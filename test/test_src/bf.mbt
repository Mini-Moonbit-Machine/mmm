fn bf(len : Int, prog : Array[Int]) -> Unit {
  let ptr = Array::make(1, 0);
  let cells = Array::make(1024 * 1024, 0);
  fn aux(pc) {
    if len <= pc {
      pc
    } else {
      let i = prog[pc];
      if i == 62 { // '>'
        ptr[0] = ptr[0] + 1;
        aux(pc + 1)
      } else if i == 60 { // '<'
        ptr[0] = ptr[0] - 1;
        aux(pc + 1)
      } else if i == 43 { // '+'
        cells[ptr[0]] = cells[ptr[0]] + 1;
        aux(pc + 1)
      } else if i == 45 { // '-'
        cells[ptr[0]] = cells[ptr[0]] - 1;
        aux(pc + 1)
      } else if i == 46 { // '.'
        print_char(cells[ptr[0]]);
        aux(pc + 1)
      } else if i == 44 { // ','
        cells[ptr[0]] = read_char();
        aux(pc + 1)
      } else if i == 91 { // '['
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
      } else if i == 93 { // ']'
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
  // let len = 128;
  // let prog = Array::make(len, 0);
  // fn read_prog(i) {
  //   let x = read_char();
  //   if x == -1 {
  //     ()
  //   } else { 
  //     if x == 10 {
  //       ()
  //     } else {
  //       prog[i] = x;
  //       read_prog(i + 1)
  //     }
  //   }
  // };
  // read_prog(0);
  // bf(len, prog)
  ()
};
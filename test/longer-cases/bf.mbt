fn read_ch_until_newline_or_eof() -> Int {
  let c = read_char();
  if c == 10 {
    -1
  } else {
    if c == 13 {
      -1
    } else {
      c
    }
  }
};

let move_left: Int = 60;
let move_right: Int = 62;
let increase: Int = 43;
let decrease: Int = 45;
let print: Int = 46;
let read: Int = 44;
let loop_start: Int = 91;
let loop_end: Int = 93;
fn bf(len : Int, prog : Array[Int]) -> Unit {
  let ptr = Array::make(1, 0);
  let cells = Array::make(1024 * 1024, 0);
  let next_pc = Array::make(1, 0);
  fn skip(pc, level: Int) {
    if len <= pc {
      pc
    } else {
      let i = prog[pc];
      if i == loop_start { // '['
        skip(pc + 1, level + 1)
      } else if i == loop_end { // ']'
        if level == 0 {
          pc
        } else {
          skip(pc + 1, level - 1)
        }
      } else {
        skip(pc + 1, level)
      }
    }
  };
  fn aux(pc) {
    if len <= pc {
      pc
    } else {
      let i = prog[pc];
      if i == move_right { // '>'
        ptr[0] = ptr[0] + 1;
        aux(pc + 1)
      } else if i == move_left { // '<'
        ptr[0] = ptr[0] - 1;
        aux(pc + 1)
      } else if i == increase { // '+'
        cells[ptr[0]] = cells[ptr[0]] + 1;
        aux(pc + 1)
      } else if i == decrease { // '-'
        cells[ptr[0]] = cells[ptr[0]] - 1;
        aux(pc + 1)
      } else if i == print { // '.'
        print_char(cells[ptr[0]]);
        aux(pc + 1)
      } else if i == read { // ','
        cells[ptr[0]] = read_ch_until_newline_or_eof();
        aux(pc + 1)
      } else if i == loop_start { // '['
        next_pc[0] = pc;
        fn lp() {
          if cells[ptr[0]] == 0 {
            next_pc[0] = skip(pc + 1, 0);
            ()
          } else {
            next_pc[0] = aux(pc + 1);
            lp()
          }
        };
        lp();
        aux(next_pc[0] + 1)
      } else if i == loop_end { // ']'
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
  let len = 1024;
  let prog = Array::make(len, 0);
  fn read_prog(i) {
    let x = read_ch_until_newline_or_eof();
    if x == -1 {
      bf(i, prog)
    } else {
      prog[i] = x;
      read_prog(i + 1)
    }
  };
  read_prog(0);
  ()
};
let pi: Double = 3.141592653589793;
let solar_mass: Double = 4.0 * pi * pi;
let days_per_year: Double = 365.24;

// planet: x, y, z, vx, vy, vz, mass

fn advance(bodies: Array[(Double, Double, Double, Double, Double, Double, Double)], dt: Double) -> Unit {
  let n = 5; // 5 body
  fn loop_i(i: Int) {
    if i <= n - 1 {
      let b = bodies[i];
      let (bx, by, bz, bvx, bvy, bvz, bm) = b;
      fn loop_j(j: Int) {
        if j <= n - 1 {
          let b2 = bodies[j];
          let (b2x, b2y, b2z, b2vx, b2vy, b2vz, b2m) = b2;
          let dx = bx - b2x;
          let dy = by - b2y;
          let dz = bz - b2z;
          let dist2 = dx * dx + dy * dy + dz * dz;
          let mag = dt / (dist2 * sqrt(dist2));

          let new_b = (bx, by, bz, bvx - dx * b2m * mag, bvy - dy * b2m * mag, bvz - dz * b2m* mag, bm);
          bodies[i] = new_b;
          let new_b2 = (b2x, b2y, b2z, b2vx - dx * b2m * mag, b2vy - dy * b2m * mag, b2vz - dz * b2m* mag, b2m);
          bodies[j] = new_b2;
          loop_j(j + 1);
        }
        else {
          ()
        };
      };
      loop_j(i + 1);
      loop_i(i + 1);
    } else {
      ()
    };
  };
  loop_i(0);
  fn update_axis(i: Int) {
    if i <= n - 1 {
      let b = bodies[i];
      let (bx, by, bz, bvx, bvy, bvz, bm) = b;
      let new_b = (bx + dt * bvx, by + dt * bvy, bz + dt * bvz, bvx, bvy, bvz, bm);
      bodies[i] = new_b;
      update_axis(i + 1);
    } else {
      ()
    };
  };
  update_axis(0);
};

fn energy(bodies: Array[(Double, Double, Double, Double, Double, Double, Double)]) -> Double {
  let n = 5; // 5 body
  let e = Array::make(1, 0.0);
  fn loop_i(i: Int) {
    if i <= n - 1 {
      let b = bodies[i];
      let (bx, by, bz, bvx, bvy, bvz, bm) = b;
      e[0] = e[0] + 0.5 * bm * (bvx * bvx + bvy * bvy + bvz * bvz);
      fn loop_j(j: Int) {
        if j <= n - 1 {
          let b2 = bodies[j];
          let (b2x, b2y, b2z, b2vx, b2vy, b2vz, b2m) = b2;
          let dx = bx - b2x;
          let dy = by - b2y;
          let dz = bz - b2z;
          let dist2 = dx * dx + dy * dy + dz * dz;
          e[0] = e[0] - (bm * b2m) / sqrt(dist2);
          loop_j(j + 1);
        }
        else {
          ()
        };
      };
      loop_j(i + 1);
      loop_i(i + 1);
    } else {
      ()
    };
  };
  loop_i(0);
  e[0]
};

fn offset_momentum(bodies: Array[(Double, Double, Double, Double, Double, Double, Double)]) -> Unit {
  let n = 5; // 5 body
  let px = Array::make(1, 0.0);
  let py = Array::make(1, 0.0);
  let pz = Array::make(1, 0.0);
  fn loop_i(i: Int) {
    if i <= n - 1 {
      let b = bodies[i];
      let (bx, by, bz, bvx, bvy, bvz, bm) = b;
      px[0] = px[0] + bvx * bm;
      py[0] = py[0] + bvy * bm;
      pz[0] = pz[0] + bvz * bm;
      loop_i(i + 1);
    } else {
      ()
    };
  };
  loop_i(0);
  let b = bodies[0];
  let (bx, by, bz, bvx, bvy, bvz, bm) = b;
  let new_b = (bx, by, bz, -px[0] / solar_mass, -py[0] / solar_mass, -pz[0] / solar_mass, bm);
  bodies[0] = new_b;
};

fn main {
  let jupiter = (
    4.84143144246472090,
    -1.16032004402742839,
    -0.103622044471123109,
    0.00166007664274403694 * days_per_year,
    0.00769901118419740425 * days_per_year,
    -0.0000690460016972063023 * days_per_year,
    0.000954791938424326609 * solar_mass);
  let saturn = (
    8.34336671824457987,
    4.12479856412430479,
    -0.403523417114321381,
    -0.00276742510726862411 * days_per_year,
    0.00499852801234917238 * days_per_year,
    0.0000230293455282290680 * days_per_year,
    0.000285885980666130812 * solar_mass);
  let uranus = (
    12.8943695621391310,
    -15.1111514016986312,
    -0.223307578892655885,
    0.00296460137564761618 * days_per_year,
    0.00237847173959480950 * days_per_year,
    -0.0000296589568540237556 * days_per_year,
    0.0000436624404335156298 * solar_mass);
  let neptune = (
    15.3796971148509165,
    -25.9193146099879641,
    0.179258772950371181,
    0.00268067772490389322 * days_per_year,
    0.00162824170038242295 * days_per_year,
    -0.0000951592254519715870 * days_per_year,
    0.0000515138902046611608 * solar_mass);
  let sun = (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, solar_mass);
  let bodies = Array::make(5, (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0));
  bodies[0] = sun;
  bodies[1] = jupiter;
  bodies[2] = saturn;
  bodies[3] = uranus;
  bodies[4] = neptune;
  offset_momentum(bodies);

  print_int(int_of_float(energy(bodies) * 1000000000.0));
  let iters = 500000;
  fn loop_i(i: Int) {
    if i <= iters {
      advance(bodies, 0.01);
      loop_i(i + 1);
    } else {
      ()
    };
  };
  loop_i(1);
  print_char(10);
  print_int(int_of_float(energy(bodies) * 1000000000.0));

}

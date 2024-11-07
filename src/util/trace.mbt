typealias MutMap[T] = @hashmap.T[String, T]

pub let tracer : Tracer = Tracer::new()

pub let t : SubTracer = tracer.new_subtracer("Main", false, false)

pub let output : SubTracer = tracer.new_subtracer("Output", true, true)

pub struct SubTracer {
  name : String
  mut output_to_console : Bool
  mut redirect_to_log : Bool
  print_buf : Buffer
  log_buf : MutMap[Array[String]]
  record_buf : MutMap[Buffer]
  tracer : Tracer
}

pub fn SubTracer::set_output_to_console(
  self : SubTracer,
  output_to_console : Bool
) -> Unit {
  self.output_to_console = output_to_console
}

pub fn SubTracer::set_redirect_to_log(
  self : SubTracer,
  redirect_to_log : Bool
) -> Unit {
  self.redirect_to_log = redirect_to_log
}

fn SubTracer::has_buffered_content(self : SubTracer) -> Bool {
  self.print_buf.to_bytes().length() != 0
}

fn SubTracer::flush(self : SubTracer) -> Unit {
  let s = self.print_buf.to_string()
  if self.output_to_console {
    println(s)
  } else if self.redirect_to_log {
    self.log(s)
  }
  self.print_buf.reset()
}

pub fn SubTracer::print[T : Show](self : SubTracer, msg : T) -> Unit {
  let s = Show::to_string(msg)
  for ch in s {
    if ch == '\n' {
      self.flush()
    } else {
      self.print_buf.write_char(ch)
    }
  }
}

pub fn SubTracer::println[T : Show](self : SubTracer, msg : T) -> Unit {
  self.print(msg)
  self.flush()
}

pub fn SubTracer::log[T : Show](self : SubTracer, msg : T) -> Unit {
  let s = "// [\{self.name}] \{Show::to_string(msg)}"
  match self.log_buf.get(self.name) {
    Some(buf) => buf.push(s)
    None => self.log_buf.set(self.name, [s])
  }
}

pub fn SubTracer::record[T : Show](
  self : SubTracer,
  item : String,
  msg : T
) -> Unit {
  match self.record_buf.get(item) {
    Some(buf) => buf.write_string(Show::to_string(msg))
    None => {
      let buf = Buffer::new()
      buf.write_string(Show::to_string(msg))
      self.record_buf.set(item, buf)
    }
  }
}

pub fn SubTracer::abort[T](self : SubTracer, msg : String) -> T {
  self.tracer.done(
    fn(name, s) { println("// [log::\{name}]\n\{s}") },
    fn(name, s) { println("// [record::\{name}]\n\{s}") },
  )
  self.println(msg)
  panic()
}

pub fn SubTracer::timeout[T](self : SubTracer, msg : String) -> T {
  while true {

  }
  panic()
}

pub struct Tracer {
  tracers : Array[(String, SubTracer)]
  log_buf : MutMap[Array[String]]
  record_buf : MutMap[Buffer]
}

pub fn Tracer::new() -> Tracer {
  Tracer::{ tracers: [], log_buf: MutMap::new(), record_buf: MutMap::new() }
}

pub fn Tracer::new_subtracer(
  self : Tracer,
  name : String,
  output_to_console : Bool,
  redirect_to_log : Bool
) -> SubTracer {
  let subtracer = SubTracer::{
    name,
    output_to_console,
    redirect_to_log,
    print_buf: Buffer::new(),
    log_buf: self.log_buf,
    record_buf: self.record_buf,
    tracer: self,
  }
  self.tracers.push((name, subtracer))
  subtracer
}

pub fn Tracer::flush(self : Tracer) -> Unit {
  for subtracer in self.tracers {
    if subtracer.1.has_buffered_content() {
      subtracer.1.flush()
    }
  }
}

pub fn Tracer::done(
  self : Tracer,
  log : (String, String) -> Unit,
  record : (String, String) -> Unit
) -> Unit {
  self.flush()
  let log_buf = self.log_buf.to_array()
  log_buf.sort_by_key(fn(x) { x.0 })
  log_buf.each(
    fn(pair) {
      let (name, buf) = pair
      let s = buf.join("\n")
      log(name, s)
    },
  )
  let record_buf = self.record_buf.to_array()
  record_buf.sort_by_key(fn(x) { x.0 })
  record_buf.each(
    fn(pair) {
      let (name, buf) = pair
      record(name, buf.to_string())
    },
  )
}

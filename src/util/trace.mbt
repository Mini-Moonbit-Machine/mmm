typealias MutMap[T] = @hashmap.T[String, T]

// enum Line {
//   Print   // a print will be output to console directly or redirect into log
//   Log     // a log will be output to log with a prefix // [TracerName]
//   Record  // a record will be output to log with a prefix ///
// }

pub let tracer : Tracer = Tracer::new()

pub let mt : SubTracer = tracer.new_subtracer("Main", false, false)

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
    None => {
      let buf = []
      buf.push(s)
      self.log_buf.set(self.name, buf)
    }
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
  self.println(msg)
  self.tracer.flush()
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
    subtracer.1.flush()
  }
}

pub fn Tracer::done(
  self : Tracer,
  log : (String, String) -> Unit,
  record : (String, String) -> Unit
) -> Unit {
  self.log_buf.each(
    fn(name, buf) {
      let s = buf.join("\n")
      log(name, s)
    },
  )
  self.record_buf.each(fn(name, buf) { record(name, buf.to_string()) })
}

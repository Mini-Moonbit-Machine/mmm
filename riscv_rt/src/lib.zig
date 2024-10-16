const std = @import("std");

var arena_alloc: std.heap.ArenaAllocator = undefined;
var allocator: std.mem.Allocator = undefined;
var stdin: std.fs.File = undefined;
var stdout: std.fs.File = undefined;

// we don't need to free the memory, so we can just use a dead large arena, and never releasing it
// until the program exits :)
pub fn init_allocator() void {
    arena_alloc = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    allocator = arena_alloc.allocator();
    // stdin = std.io.bufferedReader(std.io.getStdIn());
    stdin = std.io.getStdIn();
    stdout = std.io.getStdOut();
}

pub fn deinit_allocator() void {
    arena_alloc.deinit();
    arena_alloc = undefined;
    allocator = undefined;
}

export fn minimbt_read_int() i32 {
    var buf: [512]u8 = undefined;
    if (stdin.reader().readUntilDelimiterOrEof(buf[0..], '\n') catch return std.math.minInt(i32)) |i| {
        return std.fmt.parseInt(i32, i, 10) catch return std.math.minInt(i32);
    } else {
        return std.math.minInt(i32);
    }
}

export fn minimbt_read_char() i32 {
    var buf: [1]u8 = undefined;
    const sz = stdin.read(buf[0..]) catch return -1;
    if (sz > 0) {
        return buf[0];
    } else {
        return -1;
    }
}

export fn minimbt_print_int(i: i32) void {
    _ = stdout.writer().print("{d}", .{i}) catch 0;
}

export fn minimbt_print_endline() void {
    _ = stdout.write("\n") catch 0;
}

export fn minimbt_print_char(ch: i32) void {
    const c1: u32 = @intCast(ch);
    const c2: u8 = @truncate(c1);
    _ = stdout.writer().print("{c}", .{c2}) catch 0;
}

export fn minimbt_int_of_float(f: f64) i32 {
    return @intFromFloat(f);
}

export fn minimbt_float_of_int(i: i32) f64 {
    return @floatFromInt(i);
}

export fn minimbt_truncate(f: f64) i32 {
    return @intFromFloat(f);
}

export fn minimbt_floor(f: f64) i32 {
    return @intFromFloat(@floor(f));
}

export fn minimbt_abs_float(f: f64) f64 {
    return @abs(f);
}

export fn minimbt_sqrt(f: f64) f64 {
    return @sqrt(f);
}

export fn minimbt_sin(f: f64) f64 {
    return @sin(f);
}

export fn minimbt_cos(f: f64) f64 {
    return @cos(f);
}

export fn minimbt_atan(f: f64) f64 {
    return std.math.atan(f);
}

export fn minimbt_malloc(sz: u32) [*]u8 {
    const payload = allocator.allocWithOptions(u8, sz, @alignOf(u32), null) catch @panic("Aalloc failed!");
    return @ptrCast(payload);
}

export fn minimbt_create_array(n: u32, v: i32) [*]i32 {
    const arr = allocator.alloc(i32, n) catch @panic("Alloc failed!");
    for (arr) |*item| {
        item.* = v;
    }
    return @ptrCast(arr);
}

export fn minimbt_create_ptr_array(n: u32, init: u32) [*]u32 {
    const arr = allocator.alloc(u32, n) catch @panic("Alloc failed!");
    for (arr) |*item| {
        item.* = init;
    }
    return @ptrCast(arr);
}

export fn minimbt_create_float_array(n: u32, v: f64) [*]f64 {
    const arr = allocator.alloc(f64, n) catch @panic("Alloc failed!");
    for (arr) |*item| {
        item.* = v;
    }
    return @ptrCast(arr);
}

// Compatibility functions for mincaml
export fn mincaml_print_int(i: i32) void {
    minimbt_print_int(i);
}

export fn mincaml_print_endline() void {
    minimbt_print_endline();
}

export fn mincaml_int_of_float(f: f64) i32 {
    return minimbt_int_of_float(f);
}

export fn mincaml_float_of_int(i: i32) f64 {
    return minimbt_float_of_int(i);
}

export fn mincaml_truncate(f: f64) i32 {
    return minimbt_truncate(f);
}

export fn mincaml_floor(f: f64) i32 {
    return minimbt_floor(f);
}

export fn mincaml_abs_float(f: f64) f64 {
    return minimbt_abs_float(f);
}

export fn mincaml_sqrt(f: f64) f64 {
    return minimbt_sqrt(f);
}

export fn mincaml_sin(f: f64) f64 {
    return minimbt_sin(f);
}

export fn mincaml_cos(f: f64) f64 {
    return minimbt_cos(f);
}

export fn mincaml_atan(f: f64) f64 {
    return minimbt_atan(f);
}

export fn mincaml_malloc(sz: u32) [*]u8 {
    return minimbt_malloc(sz);
}

export fn mincaml_create_array(n: u32, v: i32) [*]i32 {
    return minimbt_create_array(n, v);
}

export fn mincaml_create_ptr_array(n: u32, init: u32) [*]u32 {
    return minimbt_create_ptr_array(n, init);
}

export fn mincaml_create_float_array(n: u32, v: f64) [*]f64 {
    return minimbt_create_float_array(n, v);
}

export fn __my_read_int() i32 {
    var buf: [512]u8 = undefined;
    if (stdin.reader().readUntilDelimiterOrEof(buf[0..], '\n') catch return std.math.minInt(i32)) |i| {
        return std.fmt.parseInt(i32, i, 10) catch return std.math.minInt(i32);
    } else {
        return std.math.minInt(i32);
    }
}

export fn __my_read_char() i32 {
    var buf: [1]u8 = undefined;
    const sz = stdin.read(buf[0..]) catch return -1;
    if (sz > 0) {
        return buf[0];
    } else {
        return -1;
    }
}

export fn __my_print_char(i: i32) void {
    const c1: u32 = @intCast(i);
    var c2: [1]u8 = undefined;
    c2[0] = @truncate(c1);
    _ = std.os.linux.write(1, &c2, 1);
}

export fn __my_print_endline() void {
    @call(.never_inline, __my_print_char, .{'\n'});
}

export fn __my_print_int(x: i32) void {
    const int_value = x;
    const value_info = @typeInfo(i32).Int;

    // The type must have the same size as `base` or be wider in order for the
    // division to work
    const min_int_bits = comptime @max(value_info.bits, 8);
    const MinInt = std.meta.Int(.unsigned, min_int_bits);

    const abs_value = @abs(int_value);
    // The worst case in terms of space needed is base 2, plus 1 for the sign
    var buf: [1 + @max(@as(comptime_int, value_info.bits), 1)]u8 = undefined;

    var a: MinInt = abs_value;
    var index: usize = buf.len;

    while (true) {
        const digit = a % 10;
        index -= 1;
        const d: u8 = @intCast(digit);
        buf[index] = d + '0';
        a /= 10;
        if (a == 0) break;
    }

    if (value_info.signedness == .signed) {
        if (int_value < 0) {
            // Negative integer
            index -= 1;
            buf[index] = '-';
        }
    }

    for (buf[index..]) |elem| {
        @call(.never_inline, __my_print_char, .{elem});
    }
}

const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;

pub const day01 = struct {
    pub fn part1(arena: Allocator, input: []const u8) Allocator.Error!void {
        _ = arena;
        _ = input;
        std.debug.print("Hello from aoc2015.zig!\n", .{});
    }

    test part1 {
        std.debug.print("Hello from test in aoc2015.day01!\n", .{});
        @panic("Hello from test in aoc2015.day01!");
    }
};

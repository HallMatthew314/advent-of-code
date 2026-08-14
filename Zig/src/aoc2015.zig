const std = @import("std");

pub const day01 = struct {
    pub fn part1() void {
        std.debug.print("Hello from aoc2015.zig!\n", .{});
    }

    test part1 {
        std.debug.print("Hello from test in aoc2015.day01!\n", .{});
        @panic("Hello from test in aoc2015.day01!");
    }
};

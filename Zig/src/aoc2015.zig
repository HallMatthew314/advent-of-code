const std = @import("std");
const Io = std.Io;
const mem = std.mem;
const Allocator = mem.Allocator;

const root = @import("root");
const printStdout = root.printStdout;

pub const day01 = struct {
    pub fn part1(_: Allocator, io: Io, input: []const u8) !void {
        const answer = calcP1(input);
        try printStdout(io, "Answer for 2015-01-1: {d}\n", .{answer});
    }

    pub fn calcP1(input: []const u8) i64 {
        var acc: i64 = 0;

        for (0..input.len) |i| {
            switch (input[i]) {
                '(' => {
                    acc += 1;
                },
                ')' => {
                    acc -= 1;
                },
                else => {},
            }
        }

        return acc;
    }

    pub fn part2(_: Allocator, io: Io, input: []const u8) !void {
        const answer = calcP2(input);

        if (answer) |a| {
            try printStdout(io, "Answer for 2015-01-2: {d}\n", .{a});
        } else {
            try printStdout(io, "Answer for 2015-01-2: elevator never enters basement\n", .{});
        }
    }

    pub fn calcP2(input: []const u8) ?usize {
        var floor: i64 = 0;

        for (0..input.len) |i| {
            switch (input[i]) {
                '(' => {
                    floor += 1;
                },
                ')' => {
                    floor -= 1;
                },
                else => {},
            }

            if (floor < 0) return i + 1;
        }

        return null;
    }
};

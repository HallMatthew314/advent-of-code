const std = @import("std");
const Init = std.process.Init;
const Io = std.Io;

const aoc2015 = @import("aoc2015.zig");

// TODO: accept problem input from stdin?
// TODO: flag to list implemented problems
const USAGE = "Usage: zigaoc <problem> <path-to-input>";
const PROBLEM_FORMAT = "Format for problem: yyyy-dd-(1/2)";
const EXAMPLES =
    \\Example usage:
    \\    zigaoc 2015-01-1 path/to/input.txt
    \\    zigaoc 2024-14-2 input.txt
;

pub fn printHelp(fd: Io.File, io: Io) !void {
    var buf: [1024]u8 = undefined;
    var w = fd.writer(io, &buf);
    const out = &w.interface;

    try out.print("{s}\n", .{USAGE});
    try out.print("\n{s}\n", .{PROBLEM_FORMAT});
    try out.print("{s}\n", .{EXAMPLES});
    try out.flush();
}

pub fn main(init: Init) !void {
    const stderr = Io.File.stderr();
    const io = init.io;

    aoc2015.day01.part1();

    try printHelp(stderr, io);
}

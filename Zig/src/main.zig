const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const process = std.process;
const Init = process.Init;
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

const Problem: type = *const fn (Allocator, []const u8) Allocator.Error!void;

fn lookupProblem(p_name: []const u8) ?Problem {
    if (mem.eql(u8, "2015-01-1", p_name)) {
        return aoc2015.day01.part1;
    } else {
        return null;
    }
}

const Settings = struct {
    problem: Problem,
    input_path: []const u8,

    pub const Error = error{
        MissingProblem,
        UnknownProblem,
        MissingInput,
    };

    pub fn parse(args: process.Args) Error!Settings {
        var args_iter = args.iterate();

        // Assuming first arg is executable name
        _ = args_iter.next() orelse return Error.MissingProblem;

        const p_name = args_iter.next() orelse
            return Error.MissingProblem;
        const problem = lookupProblem(p_name) orelse
            return Error.UnknownProblem;

        const input_path = args_iter.next() orelse
            return Error.MissingInput;

        return Settings{
            .problem = problem,
            .input_path = input_path,
        };
    }
};

const AppError = Settings.Error;

pub fn fail(err: AppError, stderr: Io.File, io: Io) !noreturn {
    switch (err) {
        error.MissingProblem => std.debug.print("ERROR: no problem name given\n", .{}),
        error.UnknownProblem => std.debug.print("ERROR: specified problem is invalid/unimplemented\n", .{}),
        error.MissingInput => std.debug.print("ERROR: no path to input file given\n", .{}),
    }

    try printHelp(stderr, io);
    process.exit(1);
}

pub fn main(init: Init) !void {
    const arena = init.arena.allocator();

    const stderr = Io.File.stderr();
    const io = init.io;

    if (Settings.parse(init.minimal.args)) |settings| {
        // TODO: read input file contents to give to problem function
        try settings.problem(arena, "garbage");
    } else |err| switch (err) {
        // please just let me catch into a noreturn block with
        // the errno as an argument, pleeeeease
        else => try fail(err, stderr, io),
    }
}

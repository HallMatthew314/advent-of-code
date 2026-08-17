const std = @import("std");
const fs = std.fs;
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

// caller owns returned memory
pub fn readWholeFile(arena: Allocator, io: Io, path: []const u8) ![]const u8 {
    const content = try Io.Dir.cwd().readFileAlloc(io, path, arena, .unlimited);

    // may or may not actually be needed
    if (!std.unicode.utf8ValidateSlice(content)) return AppError.Utf8Error;

    return content;
}

pub fn printStdout(io: Io, comptime fmt: []const u8, args: anytype) !void {
    var buf: [1024]u8 = undefined;
    var w = Io.File.stdout().writer(io, &buf);
    const out = &w.interface;

    try out.print(fmt, args);
    try out.flush();
}

pub fn printHelp(fd: Io.File, io: Io) !void {
    var buf: [1024]u8 = undefined;
    var w = fd.writer(io, &buf);
    const out = &w.interface;

    try out.print("{s}\n", .{USAGE});
    try out.print("\n{s}\n", .{PROBLEM_FORMAT});
    try out.print("{s}\n", .{EXAMPLES});
    try out.flush();
}

const Problem: type =
    *const fn (Allocator, Io, []const u8) anyerror!void;

// as soon as i learn how to use the static string map
// it is over for you all
fn lookupProblem(p_name: []const u8) ?Problem {
    if (mem.eql(u8, "2015-01-1", p_name)) {
        return aoc2015.day01.part1;
    } else if (mem.eql(u8, "2015-01-2", p_name)) {
        return aoc2015.day01.part2;
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

const AppError = Settings.Error || Io.Dir.ReadFileAllocError || error{
    Utf8Error,
};

pub fn fail(err: AppError, stderr: Io.File, io: Io) !noreturn {
    switch (err) {
        error.MissingProblem => std.debug.print("ERROR: no problem name given\n", .{}),
        error.UnknownProblem => std.debug.print("ERROR: specified problem is invalid/unimplemented\n", .{}),
        error.MissingInput => std.debug.print("ERROR: no path to input file given\n", .{}),
        error.Utf8Error => std.debug.print("ERROR: input file is not valid UTF8\n", .{}),
        // i refuse to believe there is no better way to do this
        error.Canceled, error.InputOutput, error.SystemResources, error.IsDir, error.ConnectionResetByPeer, error.NotOpenForReading, error.SocketUnconnected, error.WouldBlock, error.AccessDenied, error.LockViolation, error.Unexpected, error.FileTooBig, error.NoSpaceLeft, error.DeviceBusy, error.PermissionDenied, error.NoDevice, error.FileBusy, error.ProcessFdQuotaExceeded, error.SystemFdQuotaExceeded, error.PathAlreadyExists, error.SymLinkLoop, error.FileNotFound, error.NotDir, error.ReadOnlyFileSystem, error.NetworkNotFound, error.NameTooLong, error.BadPathName, error.PipeBusy, error.AntivirusInterference, error.FileLocksUnsupported, error.OutOfMemory, error.StreamTooLong => std.debug.print("ERROR: failed to open input file\n", .{}),
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
        const input = try readWholeFile(arena, io, settings.input_path);
        defer arena.free(input);

        try settings.problem(arena, io, input);
    } else |err| try fail(err, stderr, io);
}

const testing = std.testing;

test "2015-01-1" {
    const table = [_]struct { []const u8, i64 }{
        .{ "(())", 0 },
        .{ "()()", 0 },
        .{ "(((", 3 },
        .{ "(()(()(", 3 },
        .{ "))(((((", 3 },
        .{ "())", -1 },
        .{ "))(", -1 },
        .{ ")))", -3 },
        .{ ")())())", -3 },
    };

    for (0..table.len) |i| {
        const input, const exp = table[i];
        const actual = aoc2015.day01.calcP1(input);
        try testing.expectEqual(exp, actual);
    }
}

test "2015-01-2" {
    try testing.expectEqual(1, aoc2015.day01.calcP2(")"));
    try testing.expectEqual(5, aoc2015.day01.calcP2("()())"));
}

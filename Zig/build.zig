const std = @import("std");

pub fn build(b: *std.Build) void {
    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = b.graph.host,
    });

    const exe = b.addExecutable(.{
        .name = "zigaoc",
        .root_module = exe_mod,
    });

    b.installArtifact(exe);
}

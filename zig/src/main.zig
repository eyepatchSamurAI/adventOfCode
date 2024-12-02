const std = @import("std");

pub fn main() !void {
    var buffer: [1024]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const allocator = fba.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    var yearArg: ?u16 = null;
    var challengeNumberArg: ?u8 = null;

    _ = args.next() orelse return error.MissingBinaryName;

    while (args.next()) |arg| {
        if (std.mem.startsWith(u8, arg, "--year=")) {
            yearArg = try std.fmt.parseInt(u16, arg["--year=".len..], 10);
        } else if (std.mem.startsWith(u8, arg, "--challenge=")) {
            challengeNumberArg = try std.fmt.parseInt(u8, arg["--challenge=".len..], 10);
        }
    }

    const year = yearArg orelse return error.MissingYearArgument;
    const challengeNumber = challengeNumberArg orelse return error.MissingChallengeArgument;

    switch (year) {
        2023 => {
            const challengeYear2023 = @import("challenges/2023/main.zig");
            try challengeYear2023.runChallenge(challengeNumber);
        },
        else => std.debug.print("Year {d} not found\n", .{year}),
    }
}
